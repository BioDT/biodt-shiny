box::use(
  shiny,
  bslib[navset_tab, nav_panel],
  shinyjs[hidden, show, hide, runjs],
  leaflet,
  leaflegend[addLegendNumeric],
  terra,
  shinyWidgets[pickerInput, updatePickerInput, pickerOptions],
  waiter[Waiter],
  dplyr[bind_rows],
  echarty[ecs.render, ecs.output, ec.init],
  stats[setNames],
  utils[str],
)

box::use(
  app / logic / waiter[waiter_text],
  app / view / cwr / cwr_info[cwr_info_ui, cwr_info_server],
  app / view / cwr / cwr_contributors[cwr_contributors_ui, cwr_contributors_server],
)

mod_cwr_ui <- function(
    id,
    i18n) {
  ns <- shiny$NS(id)
  shiny$tagList(
    navset_tab(
      id = ns("tab"),
      # Info ----
      nav_panel(
        title = "Info",
        value = "Info",
        cwr_info_ui("cwr_info")
      ),
      # Map -----
      nav_panel(
        title = "Map",
        value = "Map",
        shiny$div(
          shiny$HTML("<h2>Genus and species</h2>"),
          shiny$div(
            style = "display: flex; flex-direction: column; width: 300px; gap: 10px;",
            pickerInput(
              ns("genus"),
              label = "Choose genus",
              choices = list(""),
              multiple = FALSE,
              options = list(
                `actions-box` = NULL,
                `live-search` = TRUE,
                container = "body"
              )
            ),
            pickerInput(
              ns("species"),
              label = "Choose species",
              choices = list(""),
              multiple = TRUE,
              selected = c("Sativus"),
              options = pickerOptions(
                `actions-box` = NULL,
                `live-search` = TRUE,
                container = "body",
                maxOptions = 5,
                maxOptionsText = "Comparison is restricted to maximum of 5 species at once."
              )
            ),
            pickerInput(
              ns("stress_var"),
              "Select Stress Variable:",
              choices = c(
                "None" = "None",
                "Annual Temperature" = "resampled_wc2.1_2.5m_bio_1.tif",
                "Wettest Quarter Temperature" = "resampled_wc2.1_2.5m_bio_8.tif",
                "Precipitation" = "resampled_wc2.1_2.5m_bio_12.tif",
                "Wettest Quarter Precipitation" = "resampled_wc2.1_2.5m_bio_13.tif"
              ),
              selected = "None"
            ),
            hidden(
              shiny$sliderInput(
                ns("stress_range"),
                "Select Stress Range:",
                min = 0, max = 1, value = c(0, 1)
              )
            ),
            shiny$actionButton(ns("update"), "Update Map"),
          ),
        ),
        shiny$div(
          style = "display: flex; flex-direction: row; width: 100%; gap: 10px; position: relative;",
          shiny$div(
            id = ns("map_stress_div"),
            style = "min-height: 500px !important; width: 0; transition: width 0.3s ease;",
            class = "html-fill-container",
            leaflet$leafletOutput(ns("map_stress"))
          ),
          shiny$div(
            id = ns("map_div"),
            style = "min-height: 500px !important; width: 100%; transition: width 0.3s ease; margin-left: auto;",
            class = "html-fill-container",
            leaflet$leafletOutput(ns("map"))
          )
        ),
        ecs.output(ns("tolerance_plot"))
      ),
      nav_panel(
        title = i18n$t("Contributors"),
        value = "Contributors",
        cwr_contributors_ui(
          ns("cwr_contributors"),
          i18n
        )
      )
    )
  )
}

mod_cwr_server <- function(id, i18n) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns

    msg <-
      waiter_text(message = shiny$tags$h3("Loading...",
        style = "color: #414f2f;"
      ))

    w <- Waiter$new(
      html = msg,
      color = "rgba(256,256,256,0.9)"
    )

    # Initialize reactiveValues for CWR pDT ----
    r_cwr <-
      shiny$reactiveValues(
        map = NULL,
        map_list = list(),
        rasters = NULL,
        stress_maps = list(
          `resampled_wc2.1_2.5m_bio_1.tif` = NULL,
          `resampled_wc2.1_2.5m_bio_8.tif` = NULL,
          `resampled_wc2.1_2.5m_bio_12.tif` = NULL,
          `resampled_wc2.1_2.5m_bio_13.tif` = NULL
        ),
        stress_data = NULL,
        tolerance_plot = NULL,
        stressor_range = NULL
      )

    # Path to stored files (should be set for where the files are stored)
    cwr_path <- "app/data/cwr"

    # Set pickers when CWR page opens ----
    shiny$observeEvent(
      input$tab,
      priority = 10000,
      {
        shiny$req(input$tab == "Map")

        # Get available datasets ----
        print("Map tab selected")
        # Genus
        list_genus <- list.dirs(cwr_path,
          recursive = FALSE,
          full.names = FALSE
        )

        list_genus <- list_genus |>
          setdiff("climate")

        updatePickerInput(
          inputId = "genus",
          choices = list_genus,
          selected = list_genus[1]
        )

        # Create map ----
        # r_cwr$map
        output$map <- leaflet$renderLeaflet(
          leaflet$leaflet() |>
            leaflet$addTiles()
        )

        # r_cwr$map_stress
        output$map_stress <- leaflet$renderLeaflet(
          leaflet$leaflet() |>
            leaflet$addTiles()
        )
      }
    )

    # Set species based on genus ----

    # Since genus and genus_response_curves are synced we do it just for one and set them on both pages
    shiny$observeEvent(input$genus,
      ignoreInit = TRUE,
      {
        species_list <- list.files(file.path(cwr_path, input$genus))

        updatePickerInput(
          inputId = "species",
          choices = species_list,
          selected = species_list[1]
        )
      }
    )

    # Species change logic ----
    shiny$observeEvent(input$update,
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        shiny$req(
          input$tab == "Map",
          input$species
        )

        w$show()
        for (species in input$species) {
          if (is.null(r_cwr$map_list[[species]])) {
            species_path <- file.path(
              cwr_path,
              input$genus,
              species
            )

            # Load input data ---
            r_cwr$map_list[[species]] <- terra$rast(
              file.path(
                species_path,
                "MODELS-Binarised.tif"
              )
            )
          }
        }

        # Use leaflet proxy to clear and update the raster layer
        leaflet$leafletProxy("map", session) |>
          leaflet$clearGroup("suitability")

        if (length(input$species) == 1) {
          leaflet$leafletProxy("map", session) |>
            leaflet$addRasterImage(
              x = r_cwr$map_list[[input$species]],
              colors = c("red", "green"),
              opacity = 0.6,
              project = FALSE,
              group = "suitability"
            )
          r_cwr$stress_data <- NULL
        }
        # else {
        r_cwr$stress_data <- lapply(input$species, function(species_name) {
          suitable_pixels <- which(terra$values(r_cwr$map_list[[species_name]]) == 1)
          class(suitable_pixels)
          stress_values <- terra$values(r_cwr$stress_maps[[input$stress_var]])[suitable_pixels] |>
            terra$na.omit() |>
            table() #|>
          # prop.table()

          # Create a named list with zeros for all values in the range
          range_values <- as.character(r_cwr$stressor_range[1]:r_cwr$stressor_range[2])
          filled_list <- setNames(rep(0, length(range_values)), range_values)

          # Fill in the values from stress_values where names match
          filled_list[names(stress_values)] <- stress_values

          return(
            list(
              name = species_name,
              type = "line",
              color = c("#00aa00", "#ff0000", "#0000aa")[which(species_name == input$species)],
              symbol = "none",
              showSymbol = FALSE,
              emphasis = list(disabled = TRUE),
              data = unname(as.numeric(filled_list))
            )
          )
        })
        # }

        r_cwr$tolerance_plot <- ec.init()
        r_cwr$tolerance_plot$x$opts <-
          list(
            title = list(text = "Tolerance analysis"),
            tooltip = list(
              trigger = "axis"
            ),
            xAxis = list(
              type = "category",
              boundaryGap = TRUE,
              name = "Stressor value",
              nameLocation = "middle",
              nameGap = 25,
              nameTextStyle = list(fontWeight = "bolder"),
              data = r_cwr$stressor_range[1]:r_cwr$stressor_range[2]
            ),
            yAxis = list(
              type = "value",
              boundaryGap = FALSE,
              nameLocation = "middle",
              nameGap = 40,
              nameTextStyle = list(fontWeight = "bolder"),
              min = 0
              # max = 1
            ),
            series = r_cwr$stress_data
          )
        w$hide()
      }
    )


    # Render plots ----
    shiny$observeEvent(
      r_cwr$tolerance_plot,
      {
        # str(r_cwr$tolerance_plot)
        output$tolerance_plot <- ecs.render(r_cwr$tolerance_plot)
      }
    )



    # Map UI ----
    shiny$observeEvent(
      input$stress_var,
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      priority = 1000,
      {
        if (input$stress_var == "None") {
          # Hide the stress range slider ----
          hide("stress_range")
          # Hide the stress map ----
          runjs(sprintf("document.getElementById('%s').style.width = '0';", ns("map_stress_div")))
          runjs(sprintf("document.getElementById('%s').style.width = '100%%';", ns("map_div")))

          leaflet$leafletProxy("map_stress", session) |>
            leaflet$clearGroup("stress")
        } else {
          # Show the stress range slider ----
          show("stress_range")

          # Show the stress map ----
          runjs(sprintf("document.getElementById('%s').style.width = '50%%';", ns("map_stress_div")))
          runjs(sprintf("document.getElementById('%s').style.width = '50%%';", ns("map_div")))

          # Check we have the raster data ----
          if (is.null(r_cwr$stress_maps[[input$stress_var]])) {
            w$show()
            r_cwr$stress_maps[[input$stress_var]] <- terra$rast(
              file.path(
                cwr_path,
                "climate",
                "wc2.1_2.5m",
                input$stress_var
              )
            )
            w$hide()
          }
          r_cwr$stressor_range <- terra::minmax(r_cwr$stress_maps[[input$stress_var]])

          # Update slider input ----
          shiny$updateSliderInput(session, "stress_range",
            min = r_cwr$stressor_range[1],
            max = r_cwr$stressor_range[2],
            value = c(
              r_cwr$stressor_range[1],
              r_cwr$stressor_range[2]
            )
          )

          # Plot the raster ----
          leaflet$leafletProxy("map_stress", session) |>
            leaflet$clearGroup("stress") |>
            leaflet$removeControl("legend") |>
            leaflet$addRasterImage(
              r_cwr$stress_maps[[input$stress_var]],
              colors = leaflet$colorNumeric(
                c("blue", "green", "red"),
                c(r_cwr$stressor_range[1], r_cwr$stressor_range[2]),
                na.color = "transparent"
              ),
              opacity = 0.6,
              project = FALSE,
              group = "stress"
            ) |>
            leaflet$addLegend(
              pal = leaflet$colorNumeric(
                c("blue", "green", "red"),
                c(r_cwr$stressor_range[1], r_cwr$stressor_range[2]),
                na.color = "transparent"
              ),
              values = c(r_cwr$stressor_range[1], r_cwr$stressor_range[2]),
              opacity = 0.6,
              group = "stress",
              layerId = "legend",
              position = "bottomleft"
            )
        }
      }
    )

    shiny$observeEvent(
      input$stress_range,
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        shiny$req(
          r_cwr$stress_maps[[input$stress_var]],
          input$stress_range
        )

        r_cwr$stressor_range <- terra::minmax(r_cwr$stress_maps[[input$stress_var]])

        shiny$req(
          input$stress_range[1] != r_cwr$stressor_range[1] ||
            input$stress_range[2] != r_cwr$stressor_range[2]
        )

        temp_rast <- r_cwr$stress_maps[[input$stress_var]]
        temp_rast[temp_rast < input$stress_range[1]] <- NA
        temp_rast[temp_rast > input$stress_range[2]] <- NA

        leaflet$leafletProxy("map_stress", session) |>
          leaflet$clearGroup("stress") |>
          leaflet$removeControl("legend") |>
          leaflet$addRasterImage(
            x = temp_rast,
            colors = leaflet$colorNumeric(
              c("blue", "green", "red"),
              c(r_cwr$stressor_range[1], r_cwr$stressor_range[2]),
              na.color = "transparent"
            ),
            opacity = 0.6,
            project = FALSE,
            group = "stress"
          ) |>
          leaflet$addLegend(
            pal = leaflet$colorNumeric(
              c("blue", "green", "red"),
              c(r_cwr$stressor_range[1], r_cwr$stressor_range[2]),
              na.color = "transparent"
            ),
            values = input$stress_range,
            opacity = 0.6,
            group = "stress",
            layerId = "legend",
            position = "bottomleft"
          )
      }
    )

    # Output tolerance plot
    output$tolerance_plot <- shiny$renderPlot({

    })
  })
}

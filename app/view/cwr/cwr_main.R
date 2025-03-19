box::use(
  shiny[moduleServer,
         NS,
         tagList,
         column,
         fluidRow,
         verbatimTextOutput,
         actionButton,
         observe,
         observeEvent,
         radioButtons,
         checkboxInput,
         p,
         textOutput,
         renderText,
         reactive,
         HTML,
         selectInput,
         req,
         renderUI,
         uiOutput,
         htmlOutput,
         selectizeInput,
         tags,
         reactiveValues, div, sliderInput],
  htmltools[css],
  bslib[navset_tab, nav_panel, card, card_header, card_body, layout_column_wrap],
  shinyjs[hidden, show, hide, runjs, delay],
  leaflet,
  leaflegend[addLegendNumeric],
  terra,
  shinyWidgets[pickerInput, updatePickerInput, pickerOptions],
  waiter[Waiter],
  dplyr[bind_rows],
  echarty[ecs.render, ecs.output, ec.init],
  utils[str],
  config,
)

box::use(
  app / logic / waiter[waiter_text],
  app / view / cwr / cwr_info[cwr_info_ui, cwr_info_server],
  app /
    view /
    cwr /
    cwr_contributors[cwr_contributors_ui, cwr_contributors_server],
  app /
    logic /
    cwr /
    map_utils[update_leaflet_map],
  app / logic / cwr / tolerance_plot[create_tolerance_plot],
)

mod_cwr_ui <- function(id, i18n
                       
) {
  ns <- NS(id)
 tagList(
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
        layout_column_wrap(
          width = NULL,
          fill = FALSE,
          class = "mt-2",
          style = css(grid_template_columns = "3fr 1fr"),
          card(
            class = "ms-md-3 card-shadow",
            card_header(
              tags$h2(
                class = "card_title",
                "Input map")
            ),
            card_body(
              id = ns("map_div"),
              style = "min-height: 500px !important; width: 100%; transition: width 0.3s ease; margin-left: auto;",
              class = "html-fill-container",
              leaflet$leafletOutput(ns("map"))
              # )
            ),
            hidden(
              ecs.output(ns("tolerance_plot"))
            )
          ),
          card(
            class = "me-md-3 card-shadow",
            card_header(
              tags$h2(
                class = "card_title",
                "Genus and Species")
            ),
            card_body(
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
              # Second row with stress variable and slider
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
                  sliderInput(
                    ns("stress_range"),
                    "Select Stress Range:",
                    min = 0,
                    max = 1,
                    value = c(0, 1)
                  )
                ),
                hidden(
                  checkboxInput(
                    ns("subset_suitability_map"),
                    "Subset Stressor Map with Suitability Map",
                    FALSE
                  )
                ),
                  actionButton(
                    ns("update"),
                    "Update Map",
                    style = "width: 100%;"
                  )
            ),
          ),
        ),
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
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    msg <-
      waiter_text(
        message = tags$h3("Loading...", style = "color: #414f2f;")
      )

    w <- Waiter$new(
      html = msg,
      color = "rgba(256,256,256,0.9)"
    )

    # Initialize reactiveValues for CWR pDT ----
    r_cwr <-
      reactiveValues(
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
        stressor_range = NULL,
        first_visit = TRUE,
        base_groups = list()
      )

    # Path to stored files (should be set for where the files are stored)
    cwr_path <- file.path(config$get("data_path"), "cwr")

    # Initialise maps when CWR selected
    observeEvent(
      input$tab,
      {
        req(input$tab == "Map")

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

        delay(100, {
          # Set pickers when CWR page opens ----
          list_genus <- list.dirs(
            cwr_path,
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

          if (r_cwr$first_visit) {
            r_cwr$first_visit <- FALSE
          }
        })
      }
    )

    # Set species based on genus ----

    # Since genus and genus_response_curves are synced we do it just for one and set them on both pages
    observeEvent(
      input$genus,
      ignoreInit = TRUE,
      {
        # print("Change species")
        species_list <- list.files(file.path(cwr_path, input$genus))

        updatePickerInput(
          inputId = "species",
          choices = species_list,
          selected = species_list[1]
        )
      }
    )

    # Species change logic ----
    observeEvent(
      {
        input$update
        r_cwr$first_visit
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        req(
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

        if (!is.null(input$stress_var) && input$stress_var != "None") {
          stress_map <- r_cwr$stress_maps[[input$stress_var]]
        } else {
          stress_map <- NULL
        }

        if (!is.null(input$stress_range)) {
          stress_map[stress_map < input$stress_range[1]] <- NA
          stress_map[stress_map > input$stress_range[2]] <- NA
        }

        # Update the map
        r_cwr$base_groups <- update_leaflet_map(
          r_cwr$map_list,
          input$species,
          session = session,
          stressor = input$stress_var,
          stress_map = stress_map,
          stressor_range = r_cwr$stressor_range,
          subset_suitability_map = input$subset_suitability_map
        )

        # Create tolerance plot
        r_cwr$tolerance_plot <- create_tolerance_plot(
          input$species,
          input$stress_var,
          r_cwr$stress_maps,
          r_cwr$map_list,
          r_cwr$stressor_range
        )

        w$hide()
      }
    )

    # Render tolerance plot ----
    observeEvent(
      r_cwr$tolerance_plot,
      {
        # str(r_cwr$tolerance_plot)
        output$tolerance_plot <- ecs.render(r_cwr$tolerance_plot)
      }
    )

    # Map UI ----
    observeEvent(
      input$stress_var,
      {
        req(input$stress_var, r_cwr$stress_maps)
        if (input$stress_var == "None") {
          # Hide the stress range slider ----
          hide("stress_range")
          hide("tolerance_plot")
          hide("subset_suitability_map")
          # Hide the stress map ----
          # runjs(sprintf(
          #   "document.getElementById('%s').style.width = '0';",
          #   ns("map_stress_div")
          # ))
          # runjs(sprintf(
          #   "document.getElementById('%s').style.width = '100%%';",
          #   ns("map_div")
          # ))

          # leaflet$leafletProxy("map_stress", session) |>
          #   leaflet$clearGroup("stress")
        } else {
          # Show the stress range slider ----
          show("stress_range")
          show("tolerance_plot")
          show("subset_suitability_map")

          # Show the stress map ----
          # runjs(sprintf(
          #   "document.getElementById('%s').style.width = '50%%';",
          #   ns("map_stress_div")
          # ))
          # runjs(sprintf(
          #   "document.getElementById('%s').style.width = '50%%';",
          #   ns("map_div")
          # ))

          # Add JavaScript to trigger window resize and force Leaflet maps to redraw
          # runjs(
          #   "
          #   setTimeout(function() {
          #     // Trigger a window resize event which helps Leaflet detect container changes
          #     if (window.dispatchEvent) {
          #       window.dispatchEvent(new Event('resize'));
          #     }
          #   }, 50);
          # "
          # )

          # Add a small delay to ensure DOM is updated before proceeding
          delay(100, {
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
            }
            r_cwr$stressor_range <- terra::minmax(r_cwr$stress_maps[[
              input$stress_var
            ]])

            # Update slider input ----
            updateSliderInput(
              session,
              "stress_range",
              min = r_cwr$stressor_range[1],
              max = r_cwr$stressor_range[2],
              value = c(
                r_cwr$stressor_range[1],
                r_cwr$stressor_range[2]
              )
            )

            # Update stress map ----
            # update_stress_map(
            #   stress_map = r_cwr$stress_maps[[input$stress_var]],
            #   stressor_range = r_cwr$stressor_range,
            #   session = session
            # )

            # Update stress map on common map ----
            # update_leaflet_map(
            #   r_cwr$map_list,
            #   input$species,
            #   session = session,
            #   stressor = input$stress_var,
            #   stress_map = r_cwr$stress_maps[[input$stress_var]],
            #   stressor_range = r_cwr$stressor_range
            # )

            w$hide()
          })
        }
      }
    )

   observeEvent(
      input$stress_range,
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
       req(
          input$stress_range,
          r_cwr$stress_maps[[input$stress_var]]
        )

        r_cwr$stressor_range <- terra::minmax(r_cwr$stress_maps[[
          input$stress_var
        ]])

        temp_rast <- r_cwr$stress_maps[[input$stress_var]]
        temp_rast[temp_rast < input$stress_range[1]] <- NA
        temp_rast[temp_rast > input$stress_range[2]] <- NA

        # Update stress map ----
        # update_stress_map(
        #   stress_map = temp_rast,
        #   stressor_range = r_cwr$stressor_range,
        #   session = session
        # )

        # update_leaflet_map(
        #   r_cwr$map_list,
        #   input$species,
        #   session = session,
        #   stressor = input$stress_var,
        #   stress_map = temp_rast,
        #   stressor_range = r_cwr$stressor_range
        # )
      }
    )
  })
}

box::use(
  shiny[NS, tagList, tags, HTML, icon, div, span, strong, moduleServer, fluidRow, radioButtons, column, uiOutput, sliderInput, downloadButton, reactive, req, observeEvent, updateSliderInput, renderUI, downloadHandler, reactiveVal],
  bslib[layout_sidebar, sidebar, card, card_header, card_body, card_footer],
  shinyWidgets[pickerInput, switchInput],
  leaflet[leafletOutput, renderLeaflet, leaflet, addTiles, setView, addEasyButton, easyButton, JS, leafletOptions, colorNumeric, leafletProxy, clearImages, clearControls, addRasterImage, evalFormula],
  dplyr[filter, mutate, slice],
  stringr[str_detect],
  sf[st_crs],
)

box::use(
  app / logic / ias / helper[process_raster_file, addLegend_decreasing],
)

#' @export
ias_app_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
      .summary-box {
        border: 1px solid #dee2e6; padding: 10px; text-align: center; margin: 5px; border-radius: 5px; flex: 1 1 200px;
      }
      .summary-box .fa { font-size: 24px; margin-bottom: 5px; }
      .habitat-box { background-color: #FFD700; color: #000; }
      .data-type-box { background-color: #F4A460; color: #000; }
      .time-frame-box { background-color: #FF8C00; color: #000; }
      .time-period-box { background-color: #CD5C5C; color: #fff; }
      .climate-model-box { background-color: #8B4513; color: #fff; }
      .climate-scenario-box { background-color: #D2691E; color: #fff; }
      .species-box { background-color: #DAA520; color: #000; }
    "))
    ),
    fluidRow(
      # layout_sidebar(
      # sidebar = sidebar(
      #   open = TRUE,
      #   width = "400px",
      #   title = "Select parameters",
      column(
        class = "my-3",
        style = "margin-left: 1em !important",
        width = 3,
        card(
          # Existing inputs ...
          uiOutput(
            ns("habPicker")
          ),
          radioButtons(
            ns("dataTypePicker"), "Data Type:",
            choices = c("Mean" = "mean", "Standard Deviation" = "sd", "Uncertainty" = "cov"),
            selected = "mean"
          ),
          radioButtons(
            ns("timeFramePicker"), "Time Frame:",
            choices = c("Present", "Future"),
            selected = "Present"
          ),
          uiOutput(ns("timePeriodUI")),
          uiOutput(ns("climateModelUI")),
          uiOutput(ns("climateScenarioUI")),
          switchInput(
            width = "100%", size = "normal",
            inputId = ns("showSpecies"), label = "Species",
            onLabel = "ON", offLabel = "OFF", value = FALSE
          ),
          uiOutput(ns("speciesPicker")),

          # ---------------- NEW: Dynamic slider for threshold --------------
          sliderInput(
            ns("valueThreshold"),
            label = "Display values above:",
            min   = 0,
            max   = 1,
            value = 0, # initial value (will be overridden dynamically)
            step  = 0.01
          ),
          # -----------------------------------------------------------------

          downloadButton(ns("downloadTif"), "Download TIFF File")
        )
      ),
      column(
        8,
        class = "m-3",
        card(
          height = 650,
          full_screen = TRUE,
          card_header("pDT-IAS"),
          card_body(leafletOutput(ns("rasterMap"))),
          card_footer(class = "fs-6", uiOutput(ns("selectedOptions")))
        )
      )
    )
  )
}

#' @export
ias_app_server <- function(id, tab_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    predictions_summary <- reactiveVal(NULL)

    # UI dependent on predictions_summary ----
    output$habPicker <- renderUI(
      if (!is.null(predictions_summary())) {
        pickerInput(
          ns("habNamePicker"), "Habitat:",
          choices = sort(unique(predictions_summary()$hab_name)),
          selected = NULL, multiple = FALSE
        )
      } else {
        NULL
      }
    )

    output$speciesPicker <- renderUI(
      if (!is.null(predictions_summary()) && isTRUE(input$showSpecies)) {
        pickerInput(
          ns("speciesNamePicker"), "Select species:",
          choices = sort(unique(predictions_summary()$species_name)),
          selected = NULL, multiple = FALSE
        )
      } else {
        NULL
      }
    )

    # Load resources when user access IAS tab ----
    observeEvent(
      tab_selected(),
      {
        print(tab_selected)
        req(tab_selected() == "ias_app")

        # Define the base URL for the OPeNDAP server
        base_url <- "http://opendap.biodt.eu/ias-pdt/predictions"

        # Load the Predictions_Summary.RData from the OPeNDAP server
        load(url(paste0(base_url, "/Predictions_Summary.RData")))
        Predictions_Summary |>
          predictions_summary()

        # Update the paths to point to the remote OPeNDAP files
        predictions_summary() |>
          mutate(
            tif_path_mean = paste0(base_url, "/", tif_path_mean),
            tif_path_cov  = paste0(base_url, "/", tif_path_cov),
            tif_path_sd   = paste0(base_url, "/", tif_path_sd)
          ) |>
          predictions_summary()

        # Mapping climate model to folder name (unchanged logic)
        climate_model_mapping <- list(
          "Current"       = "Current",
          "Ensemble"      = "Ensemble",
          "GFDL-ESM4"     = "GFDL_ESM4",
          "IPSL-CM6A-LR"  = "IPSL_CM6A_LR",
          "MPI-ESM1-2-HR" = "MPI_ESM1_2_HR",
          "MRI-ESM2-0"    = "MRI_ESM2_0",
          "UKESM1-0-LL"   = "UKESM1_0_LL"
        )
      }
    )


    # -- 1. Filter predictions based on user input
    filtered_summary <- reactive({
      req(input$timeFramePicker, input$habNamePicker, input$dataTypePicker, predictions_summary())
      if (input$timeFramePicker == "Present") {
        climate_model_folder <- climate_model_mapping[["Current"]]
        df <- predictions_summary() |>
          filter(
            time_period == "1981-2010",
            climate_model == "Current",
            climate_scenario == "Current",
            hab_name == input$habNamePicker,
            str_detect(tif_path_mean, climate_model_folder)
          )
      } else {
        req(input$timePeriodPicker, input$climateModelPicker, input$climateScenarioPicker)
        climate_model_folder <- climate_model_mapping[[input$climateModelPicker]]
        df <- predictions_summary() |>
          filter(
            time_period == input$timePeriodPicker,
            climate_model == input$climateModelPicker,
            climate_scenario == input$climateScenarioPicker,
            hab_name == input$habNamePicker,
            str_detect(tif_path_mean, climate_model_folder)
          )
      }
      if (input$showSpecies && !is.null(input$speciesNamePicker)) {
        df <- df |> filter(species_name == input$speciesNamePicker)
      } else {
        df <- df |> filter(str_detect(tif_path_mean, "SR_mean"))
      }
      if (input$dataTypePicker == "mean") {
        df <- df |> mutate(tif_path = tif_path_mean)
      } else if (input$dataTypePicker == "cov") {
        df <- df |> mutate(tif_path = tif_path_cov)
      } else if (input$dataTypePicker == "sd") {
        df <- df |> mutate(tif_path = tif_path_sd)
      } else {
        stop("Invalid data type selected")
      }
      df
    })

    # # -- 2. Default raster if none is found
    default_raster_file <- reactive({
      req(input$dataTypePicker)
      if (input$dataTypePicker == "mean") {
        default_row <- predictions_summary() |>
          filter(
            time_period == "1981-2010",
            climate_model == "Current",
            climate_scenario == "Current",
            str_detect(tif_path_mean, "SR_mean")
          ) |>
          slice(1)
        return(default_row$tif_path_mean)
      } else if (input$dataTypePicker == "cov") {
        default_row <- predictions_summary() |>
          filter(
            time_period == "1981-2010",
            climate_model == "Current",
            climate_scenario == "Current",
            str_detect(tif_path_cov, "SR_cov")
          ) |>
          slice(1)
        return(default_row$tif_path_cov)
      } else if (input$dataTypePicker == "sd") {
        default_row <- predictions_summary() |>
          filter(
            time_period == "1981-2010",
            climate_model == "Current",
            climate_scenario == "Current",
            str_detect(tif_path_sd, "SR_sd")
          ) |>
          slice(1)
        return(default_row$tif_path_sd)
      } else {
        stop("Invalid data type selected")
      }
    })

    # -- 3. Load/Process the selected GeoTIFF
    raster_data <- reactive({
      req(filtered_summary())
      summary_row <- filtered_summary()
      if (nrow(summary_row) == 0) {
        default_file <- default_raster_file()
        return(process_raster_file(default_file))
      }
      req(nrow(summary_row) == 1)
      cat("Loading file:", summary_row$tif_path[1], "\n")
      process_raster_file(summary_row$tif_path[1])
    })

    # -- 4. Mask the raster based on the threshold from the slider
    filtered_raster <- reactive({
      req(raster_data())
      r <- raster_data()

      # Only show cells where the value is >= user threshold
      r[r < input$valueThreshold] <- NA
      r
    })

    # -- 5. Render the base Leaflet map only once
    output$rasterMap <- renderLeaflet({
      leaflet(options = leafletOptions(zoomSnap = 0.05, zoomDelta = 0.05)) |>
        addTiles() |>
        setView(lng = 10, lat = 50, zoom = 4) |>
        addEasyButton(
          easyButton(
            icon = "fa-house",
            title = "Reset Zoom",
            onClick = JS("function(btn, map){ map.setView([50, 10], 4); }")
          )
        )
    })

    # -- 6. Dynamic update of slider min/max when a new map loads
    observeEvent(raster_data(), {
      req(raster_data())
      r <- raster_data()
      rMin <- min(r[], na.rm = TRUE)
      rMax <- max(r[], na.rm = TRUE)

      # Update the slider to reflect the new raster's range
      updateSliderInput(session, "valueThreshold",
        min   = round(rMin, 4),
        max   = round(rMax, 4),
        step  = round((rMax - rMin) / 100, 4), # Arbitrary step calculation
        value = round(rMin, 4) # Start threshold at the min value
      )
    })

    # -- 7. Plot the thresholded raster
    observeEvent(filtered_raster(), {
      req(filtered_raster())
      r_thr <- filtered_raster()
      # We'll color by the entire underlying data range or by the thresholded range
      # Here, let's do the entire original range for the legend domain:
      full_range <- range(raster_data()[], na.rm = TRUE)

      pal <- colorNumeric("plasma", domain = full_range, na.color = "transparent")

      # Determine legend title
      legend_title <- ""
      if (input$dataTypePicker == "mean") {
        if (input$showSpecies && !is.null(input$speciesNamePicker)) {
          legend_title <- "Probability of occurrence"
        } else {
          legend_title <- "Level of invasion"
        }
      } else if (input$dataTypePicker == "sd") {
        legend_title <- "Standard Deviation"
      } else if (input$dataTypePicker == "cov") {
        legend_title <- "Uncertainty"
      }

      leafletProxy("rasterMap") |>
        clearImages() |>
        clearControls() |>
        addRasterImage(
          r_thr,
          colors = pal,
          opacity = 0.85,
          project = TRUE
        ) |>
        addLegend_decreasing(
          pal = pal,
          values = full_range, # or r_thr[] if you prefer
          title = legend_title,
          position = "bottomright",
          decreasing = TRUE
        )
    })

    # -- 8. UI outputs for time/future scenario
    output$timePeriodUI <- renderUI({
      if (input$timeFramePicker == "Present") {
        NULL
      } else {
        radioButtons(ns("timePeriodPicker"), "Time period:",
          choices = setdiff(sort(unique(predictions_summary()$time_period)), "1981-2010"),
          selected = "2011-2040"
        )
      }
    })
    output$climateModelUI <- renderUI({
      if (input$timeFramePicker == "Present") {
        NULL
      } else {
        pickerInput(
          ns("climateModelPicker"), "Climate model:",
          choices = setdiff(sort(unique(predictions_summary()$climate_model)), "Current"),
          selected = "Ensemble",
          multiple = FALSE
        )
      }
    })
    output$climateScenarioUI <- renderUI({
      if (input$timeFramePicker == "Present") {
        NULL
      } else {
        pickerInput(
          ns("climateScenarioPicker"), "Climate scenario:",
          choices = setdiff(sort(unique(predictions_summary()$climate_scenario)), "Current"),
          selected = "ssp126",
          multiple = FALSE
        )
      }
    })

    # -- 9. Selected options below the map
    output$selectedOptions <- renderUI({
      boxes <- list()

      # Habitat
      boxes[[length(boxes) + 1]] <- column(
        width = 4,
        div(
          class = "summary-box habitat-box",
          icon("leaf"), span("Habitat: ", strong(input$habNamePicker))
        )
      )
      # Data Type
      boxes[[length(boxes) + 1]] <- column(
        width = 4,
        div(
          class = "summary-box data-type-box",
          icon("chart-bar"),
          span("Data Type: ", strong(switch(input$dataTypePicker,
            "mean" = "Mean",
            "sd" = "Standard Deviation",
            "cov" = "Uncertainty"
          )))
        )
      )
      # Time Frame
      boxes[[length(boxes) + 1]] <- column(
        width = 4,
        div(
          class = "summary-box time-frame-box",
          icon("calendar"), span("Time Frame: ", strong(input$timeFramePicker))
        )
      )
      # Time Period
      time_period <- if (input$timeFramePicker == "Future" && !is.null(input$timePeriodPicker)) {
        input$timePeriodPicker
      } else {
        "1981-2010"
      }
      boxes[[length(boxes) + 1]] <- column(
        width = 4,
        div(
          class = "summary-box time-period-box",
          icon("clock"), span("Time Period: ", strong(time_period))
        )
      )
      # Climate Model
      climate_model <- if (input$timeFramePicker == "Future" && !is.null(input$climateModelPicker)) {
        input$climateModelPicker
      } else {
        "Current"
      }
      boxes[[length(boxes) + 1]] <- column(
        width = 4,
        div(
          class = "summary-box climate-model-box",
          icon("cloud"), span("Climate Model: ", strong(climate_model))
        )
      )
      # Climate Scenario
      climate_scenario <- if (input$timeFramePicker == "Future" && !is.null(input$climateScenarioPicker)) {
        input$climateScenarioPicker
      } else {
        "Current"
      }
      boxes[[length(boxes) + 1]] <- column(
        width = 4,
        div(
          class = "summary-box climate-scenario-box",
          icon("thermometer-half"), span("Climate Scenario: ", strong(climate_scenario))
        )
      )
      # Species
      if (input$showSpecies && !is.null(input$speciesNamePicker)) {
        boxes[[length(boxes) + 1]] <- column(
          width = 4,
          div(
            class = "summary-box species-box",
            icon("bug"), span("Species: ", strong(input$speciesNamePicker))
          )
        )
      }
      fluidRow(style = "display: flex; flex-wrap: wrap;", boxes)
    })

    # -- 10. Download Handler
    output$downloadTif <- downloadHandler(
      filename = function() {
        habitat <- ifelse(!is.null(input$habNamePicker), input$habNamePicker, "All_Habitats")
        habitat <- gsub(" ", "_", habitat)

        data_type <- switch(input$dataTypePicker,
          "mean" = "Mean",
          "sd"   = "SD",
          "cov"  = "Uncertainty"
        )

        time_frame <- input$timeFramePicker
        if (time_frame == "Future") {
          time_period <- input$timePeriodPicker
          climate_model <- input$climateModelPicker
          climate_scenario <- input$climateScenarioPicker
        } else {
          time_period <- "1981-2010"
          climate_model <- "Current"
          climate_scenario <- "Current"
        }

        species <- if (input$showSpecies && !is.null(input$speciesNamePicker)) {
          gsub(" ", "_", input$speciesNamePicker)
        } else {
          NULL
        }

        components <- c(
          "pDT-IAS",
          habitat,
          data_type,
          time_frame,
          time_period,
          climate_model,
          climate_scenario,
          species
        )
        components <- components[!sapply(components, is.null)]
        components <- gsub("[^A-Za-z0-9_]", "_", components)
        filename <- paste(components, collapse = "_")
        paste0(filename, ".tif")
      },
      content = function(file) {
        # Downloads the *original* file from the server
        # If you prefer to let the user download the thresholded raster,
        # you'd do something like:
        #
        #   thr_r <- filtered_raster()
        #   tmpfile <- tempfile(fileext=".tif")
        #   raster::writeRaster(thr_r, tmpfile)
        #   file.copy(tmpfile, file)
        #
        summary_row <- filtered_summary()
        if (nrow(summary_row) == 0) {
          file_to_send <- default_raster_file()
        } else {
          file_to_send <- summary_row$tif_path[1]
        }
        file.copy(from = file_to_send, to = file)
      },
      contentType = "application/octet-stream"
    )
  })
}

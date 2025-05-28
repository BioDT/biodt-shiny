box::use(
  shiny[
    NS,
    tagList,
    tags,
    HTML,
    icon,
    div,
    span,
    strong,
    moduleServer,
    fluidRow,
    radioButtons,
    column,
    uiOutput,
    sliderInput,
    downloadButton,
    reactive,
    req,
    observeEvent,
    updateSliderInput,
    renderUI,
    downloadHandler,
    reactiveVal,
    selectInput,
    conditionalPanel,
    actionButton,
    absolutePanel,
    eventReactive,
    observe,
    updateSelectInput,
    isolate,
    showNotification,
    updateRadioButtons
  ],
  bslib[layout_sidebar, sidebar, card, card_header, card_body, card_footer],
  shinyWidgets[actionBttn, pickerInput, switchInput, updatePickerInput],
  leaflet[
    leafletOutput,
    renderLeaflet,
    leaflet,
    addTiles,
    setView,
    addEasyButton,
    easyButton,
    JS,
    leafletOptions,
    colorNumeric,
    leafletProxy,
    clearImages,
    clearControls,
    addRasterImage,
    evalFormula,
    addLegend,
    addPopups,
    clearPopups
  ],
  dplyr[filter, mutate, slice, `%>%`, case_when, if_else],
  stringr[str_detect],
  sf[st_crs],
  utils[str],
  terra[mask, ifel],
  shinyalert[shinyalert],
  htmltools[htmlEscape],
  tibble[tibble],
)

box::use(
  app /
    logic /
    ias /
    helper[
      process_raster_file,
      addLegend_decreasing,
      url.exists,
      habitat_mapping,
      get_available_versions,
      check_valid_version,
      get_base_url,
      get_species_file_from_pa
    ],
)

#' @export
ias_app_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 3,
        card(
          div(
            class = "sidebar-content",

            selectInput(
              inputId = ns("pdtVersion"),
              label = "Select pDT version:",
              choices = NULL
            ),

            radioButtons(
              inputId = ns("dataMode"),
              label = "Select data mode:",
              choices = c("Projection", "Distribution"),
              selected = "Projection"
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Projection'", ns("dataMode")),
              tagList(
                pickerInput(
                  ns("habitat"),
                  "Select habitat type:",
                  choices = habitat_mapping,
                  selected = NULL
                ),

                radioButtons(
                  ns("timeFramePicker"),
                  "Select time frame:",
                  choices = c("Present", "Future"),
                  selected = NULL
                ),

                uiOutput(ns("dataTypeUI")),

                conditionalPanel(
                  condition = sprintf("input['%s'] == 'Future'", ns("timeFramePicker")),
                  tagList(
                    radioButtons(
                      ns("timePeriodPicker"),
                      "Select time period:",
                      choices = c("2011-2040", "2041-2070", "2071-2100")
                    ),

                    div(
                      style = "display: flex; align-items: center;",
                      pickerInput(
                        ns("climateModelPicker"),
                        "Select climate model:",
                        choices = NULL,
                        selected = NULL,
                        multiple = FALSE
                      ),
                      actionBttn(
                        inputId = ns("info_climate_models"),
                        label = NULL,
                        icon = icon("info-circle"),
                        color = "warning",
                        size = "xs"
                      )
                    ),

                    div(
                      style = "display: flex; align-items: center;",
                      pickerInput(
                        ns("climateScenarioPicker"),
                        "Select climate scenario:",
                        choices = NULL,
                        selected = NULL,
                        multiple = FALSE
                      ),
                      actionBttn(
                        inputId = ns("info_climate_scenarios"),
                        label = NULL,
                        icon = icon("info-circle"),
                        color = "warning",
                        size = "xs"
                      )
                    )
                  )
                ),

                switchInput(
                  ns("showSpecies"),
                  label = "Species",
                  onLabel = "ON",
                  offLabel = "OFF",
                  value = FALSE
                ),

                conditionalPanel(
                  condition = sprintf("input['%s'] == true", ns("showSpecies")),
                  uiOutput(ns("speciesInputUI"))
                ),

                sliderInput(
                  ns("valueRange"),
                  label = "Filter values between:",
                  min = 0,
                  max = 10,
                  value = c(0, 10),
                  step = 0.1
                )
              )
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Distribution'", ns("dataMode")),
              tagList(
                pickerInput(
                  ns("habitatDist"),
                  "Select habitat type:",
                  choices = habitat_mapping,
                  selected = NULL
                ),

                switchInput(
                  ns("showSpeciesDist"),
                  label = "Species",
                  onLabel = "ON",
                  offLabel = "OFF",
                  value = FALSE
                ),

                conditionalPanel(
                  condition = sprintf("input['%s'] == true", ns("showSpeciesDist")),
                  uiOutput(ns("speciesDistInputUI"))
                ),

                radioButtons(
                  inputId = ns("obsType"),
                  label = "Observed data type:",
                  choices = c("Observed values" = "full", "Modeled values" = "model"),
                  selected = "full"
                )
              )
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Projection'", ns("dataMode")),
              actionButton(ns("loadMap"), "Load map", icon = icon("globe-europe"), class = "btn-warning")
            ),

            conditionalPanel(
              condition = sprintf("input['%s'] == 'Distribution'", ns("dataMode")),
              actionButton(ns("loadObserved"), "Load observed map", icon = icon("map"), class = "btn-warning")
            ),

            downloadButton(ns("downloadTif"), "Download TIFF File", class = "btn-danger")
          )
        )
      ),
      column(
        width = 9,
        card(
          height = 650,
          full_screen = TRUE,
          card_header("Map Viewer"),
          card_body(
            leafletOutput(ns("rasterMap"), height = "100%"),
            absolutePanel(
              id = ns("opacityControl"),
              class = "panel panel-default",
              fixed = TRUE,
              draggable = FALSE,
              top = 120,
              right = 35,
              width = 150,
              height = "auto",
              style = "
                z-index: 9999;
                padding: 10px;
                background-color: rgba(248, 249, 250, 0.9);
                border-radius: 8px;
                box-shadow: 0px 0px 10px rgba(0,0,0,0.2);
                display: none;
                font-size: 12px;
              ",
              sliderInput(ns("opacitySlider"), "Transparency:", min = 0, max = 1, value = 1, step = 0.1)
            )
          ),
          card_footer(
            class = "fs-6",
            uiOutput(ns("selectedOptions"))
          )
        )
      )
    )
  )
}

#' @export
ias_app_server <- function(id, tab_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    available_versions <- reactiveVal(NULL)

    # climate model mapping
    climate_model_folder <- list(
      "Current" = "Current",
      "Ensemble" = "Ensemble",
      "GFDL-ESM4" = "GFDL_ESM4",
      "IPSL-CM6A-LR" = "IPSL_CM6A_LR",
      "MPI-ESM1-2-HR" = "MPI_ESM1_2_HR",
      "MRI-ESM2-0" = "MRI_ESM2_0",
      "UKESM1-0-LL" = "UKESM1_0_LL"
    )

    # legend caption helper
    get_projection_legend_title <- function(dataType, species_on, species_selected) {
      switch(
        dataType,
        mean = if (species_on && !is.null(species_selected)) "Probability of occurrence" else "Level of invasion",
        sd = "Standard deviation",
        cov = "Coefficient of variation",
        anomaly = "Prediction anomaly",
        "Raster value"
      )
    }

    # Track available and selected versions from OPeNDAP
    observeEvent(
      tab_selected(),
      {
        req(tab_selected() == "IAS App")
        available_versions(get_available_versions())
      }
    )

    selected_version <- reactiveVal(NULL)

    observe({
      req(available_versions())
      updateSelectInput(
        session,
        "pdtVersion",
        choices = available_versions(),
        selected = selected_version()
      )
    })

    # Automatically select the best version
    observe({
      req(available_versions())

      versions <- available_versions()
      req(length(versions) > 0)

      try_version <- versions[1]
      if (check_valid_version(try_version)) {
        selected_version(try_version)
      } else if (length(versions) >= 2 && check_valid_version(versions[2])) {
        selected_version(versions[2])
        shinyalert::shinyalert(
          title = "Fallback to Previous Version",
          text = paste("Version", try_version, "was invalid. Using fallback:", versions[2]),
          type = "warning"
        )
      } else {
        selected_version(try_version)
        shinyalert::shinyalert(
          title = "Warning",
          text = paste("No valid data found in version", try_version),
          type = "warning"
        )
      }
    })

    observeEvent(input$pdtVersion, {
      req(input$pdtVersion)
      version_to_try <- input$pdtVersion

      if (check_valid_version(version_to_try)) {
        # make it the active version used by get_base_url()
        selected_version(version_to_try)
      } else {
        shinyalert::shinyalert(
          title = "Version not fully deployed yet",
          text = paste("No valid data found in pDT version", version_to_try),
          type = "error"
        )

        # roll the drop-down back to the last working version
        updateSelectInput(session, "pdtVersion", selected = selected_version())
      }
    })

    # Base URL derived from selected version
    base_url <- reactive({
      req(selected_version())
      get_base_url(selected_version())
    })

    # Reactive values and states
    predictions_summary <- reactiveVal(NULL)
    species_data <- reactiveVal(NULL)

    observeEvent(
      {
        input$pdtVersion
        input$habitat
        tab_selected()
      },
      {
        req(input$pdtVersion, input$habitat, tab_selected() == "IAS App")

        file_url <- paste0(
          get_base_url(selected_version()),
          "outputs/",
          input$habitat,
          "/predictions/Prediction_Summary_Shiny.RData"
        )

        cat("Trying to load prediction summary from URL:\n", file_url, "\n")

        tmp <- new.env()
        tryCatch(
          {
            load(url(file_url), envir = tmp)
            ps <- tmp$Prediction_Summary_Shiny
            print(str(ps))

            # Normalize paths
            ps <- ps %>%
              mutate(
                tif_path_mean = case_when(
                  climate_model == "Ensemble" ~
                    paste0(gsub("-", "_", time_period), "_", climate_scenario, "_Ensemble/SR_mean.tif"),
                  TRUE ~ .data[["tif_path_mean"]]
                ),
                tif_path_sd = case_when(
                  climate_model == "Ensemble" ~
                    paste0(gsub("-", "_", time_period), "_", climate_scenario, "_Ensemble/SR_sd.tif"),
                  TRUE ~ .data[["tif_path_sd"]]
                ),
                tif_path_cov = case_when(
                  climate_model == "Ensemble" ~
                    paste0(gsub("-", "_", time_period), "_", climate_scenario, "_Ensemble/SR_cov.tif"),
                  TRUE ~ .data[["tif_path_cov"]]
                ),
                tif_path_anomaly = case_when(
                  climate_model == "Ensemble" ~
                    paste0(gsub("-", "_", time_period), "_", climate_scenario, "_Ensemble/SR_anomaly.tif"),
                  TRUE ~ .data[["tif_path_anomaly"]]
                )
              )

            habitat_code <- input$habitat

            ps <- ps %>%
              mutate(
                tif_path_mean = paste0(
                  get_base_url(selected_version()),
                  "outputs/",
                  habitat_code,
                  "/predictions/",
                  tif_path_mean
                ),
                tif_path_cov = paste0(
                  get_base_url(selected_version()),
                  "outputs/",
                  habitat_code,
                  "/predictions/",
                  tif_path_cov
                ),
                tif_path_sd = paste0(
                  get_base_url(selected_version()),
                  "outputs/",
                  habitat_code,
                  "/predictions/",
                  tif_path_sd
                ),
                tif_path_anomaly = if_else(
                  is.na(tif_path_anomaly),
                  NA_character_,
                  paste0(get_base_url(selected_version()), "outputs/", habitat_code, "/predictions/", tif_path_anomaly)
                )
              )

            predictions_summary(ps)
            species_data(ps)
          },
          error = function(e) {
            predictions_summary(NULL)
            species_data(NULL)
            message("Error loading prediction summary: ", e$message)
            showNotification("Error loading prediction summary file.", type = "error")
          }
        )
      }
    )

    # Update Climate Model picker
    observeEvent(predictions_summary(), {
      req(predictions_summary())

      climate_models <- predictions_summary()$climate_model
      climate_models <- setdiff(unique(climate_models), "Current")

      ordered_climate_models <- c(
        "Ensemble",
        setdiff(climate_models, "Ensemble")
      )

      updatePickerInput(session, "climateModelPicker", choices = ordered_climate_models, selected = "Ensemble")
    })

    # Update Climate Scenario picker
    observeEvent(predictions_summary(), {
      req(predictions_summary())

      climate_scenarios <- predictions_summary()$climate_scenario
      climate_scenarios <- setdiff(unique(climate_scenarios), "Current")

      updatePickerInput(session, "climateScenarioPicker", choices = climate_scenarios, selected = climate_scenarios[1])
    })

    # Update Time Period picker
    observeEvent(predictions_summary(), {
      req(predictions_summary())

      periods <- predictions_summary()$time_period
      periods <- setdiff(unique(periods), "1981-2010")

      updateRadioButtons(session, "timePeriodPicker", choices = periods, selected = periods[1])
    })

    output$speciesInputUI <- renderUI({
      req(species_data())

      species_list <- species_data()$species_name
      species_list <- species_list[!is.na(species_list)]
      species_list <- sort(unique(species_list))

      shinyWidgets::pickerInput(
        inputId = ns("speciesNamePicker"),
        label = "Select species:",
        choices = species_list,
        selected = NULL,
        multiple = FALSE,
        options = list(`live-search` = TRUE, `none-selected-text` = "Select species"),
        choicesOpt = list(
          content = lapply(species_list, function(name) {
            HTML(paste0("<i>", htmltools::htmlEscape(name), "</i>"))
          })
        )
      )
    })

    output$speciesDistInputUI <- renderUI({
      req(input$habitatDist)
      habitat_code <- input$habitatDist

      file_url <- paste0(base_url(), "outputs/", habitat_code, "/predictions/Prediction_Summary_Shiny.RData")

      tmp <- new.env()
      tryCatch(
        {
          load(url(file_url), envir = tmp)
          df <- tmp$Prediction_Summary_Shiny

          df <- df |>
            dplyr::filter(!is.na(ias_id), !is.na(species_name)) |>
            dplyr::distinct(ias_id, species_name)

          species_list <- sort(unique(df$species_name))
          pickerInput(
            inputId = ns("speciesNamePickerDist"),
            label = "Select species:",
            choices = df$ias_id,
            selected = NULL,
            multiple = FALSE,
            options = list(`live-search` = TRUE, `none-selected-text` = "Select species"),
            choicesOpt = list(
              content = lapply(species_list, function(name) {
                HTML(paste0("<i>", htmltools::htmlEscape(name), "</i>"))
              })
            )
          )
        },
        error = function(e) {
          showNotification("Unable to load species for the selected habitat in distribution mode.", type = "error")
          NULL
        }
      )
    })

    output$dataTypeUI <- renderUI({
      choices <- c(
        "Mean" = "mean",
        "Uncertainty (standard deviation)" = "sd",
        "Uncertainty (coefficient of variation)" = "cov"
      )

      if (input$timeFramePicker == "Future") {
        choices <- c(choices, "Prediction anomaly" = "anomaly")
      }

      radioButtons(
        inputId = ns("dataTypePicker"),
        label = "Select model output type:",
        choices = choices,
        selected = isolate(input$dataTypePicker %||% "mean")
      )
    })

    # Reactive: Filtered summary
    filtered_summary <- eventReactive(input$loadMap, {
      req(predictions_summary())
      df <- predictions_summary()
      field <- switch(
        input$dataTypePicker,
        mean = "tif_path_mean",
        sd = "tif_path_sd",
        cov = "tif_path_cov",
        anomaly = "tif_path_anomaly"
      )
      df <- df %>% filter(hab_name == names(habitat_mapping)[habitat_mapping == input$habitat])

      if (input$timeFramePicker == "Present") {
        df <- df %>%
          filter(time_period == "1981-2010", climate_model == "Current", climate_scenario == "Current")
      } else {
        df <- df %>%
          filter(
            time_period == input$timePeriodPicker,
            climate_model == input$climateModelPicker,
            climate_scenario == input$climateScenarioPicker
          )
      }

      # Handle species-specific rasters
      if (input$showSpecies && !is.null(input$speciesNamePicker)) {
        df <- df %>% filter(species_name == input$speciesNamePicker)
        species_id <- unique(df$ias_id)
        scenario_folder <- if (input$timeFramePicker == "Future") {
          paste0(
            gsub("-", "_", input$timePeriodPicker),
            "_",
            input$climateScenarioPicker,
            "_",
            climate_model_folder[[input$climateModelPicker]]
          )
        } else {
          "Current"
        }
        file_url <- paste0(
          get_base_url(selected_version()),
          "outputs/",
          input$habitat,
          "/predictions/",
          scenario_folder,
          "/",
          species_id,
          "_",
          input$dataTypePicker,
          ".tif"
        )

        df <- df %>% mutate(tif_path = file_url)
      } else {
        df <- df %>%
          filter(is.na(species_name), str_detect(.data[[field]], "SR_")) %>%
          mutate(tif_path = .data[[field]])
      }

      df
    })

    observed_summary <- eventReactive(
      {
        input$loadObserved
        input$habitatDist
        input$speciesNamePickerDist
        input$obsType
      },
      {
        req(input$habitatDist, input$obsType)

        tif_name <- if (input$obsType == "full") "SR_full.tif" else "SR_model.tif"
        file_url <- paste0(base_url(), "outputs/", input$habitatDist, "/observed_distribution/", tif_name)

        if (input$showSpeciesDist && !is.null(input$speciesNamePickerDist)) {
          species_id <- input$speciesNamePickerDist
          tif_name_species <- get_species_file_from_pa(
            input$habitatDist,
            species_id,
            input$obsType,
            base_url()
          )
          if (!is.null(tif_name_species))
            file_url <- paste0(base_url(), "outputs/", input$habitatDist, "/observed_distribution/", tif_name_species)
        }

        tibble::tibble(tif_path = file_url)
      }
    )

    # Raster loading
    raster_data <- reactive({
      req(filtered_summary())
      row <- filtered_summary()
      if (nrow(row) == 0 || is.na(row$tif_path[1])) return(NULL)
      process_raster_file(row$tif_path[1])
    })

    # Reactive: Load raster for Distribution
    observed_raster <- reactive({
      row <- observed_summary()
      if (nrow(row) == 0 || is.na(row$tif_path[1])) return(NULL)
      process_raster_file(row$tif_path[1])
    })

    # Raster filtering
    filtered_raster <- reactive({
      req(raster_data())
      r <- raster_data()
      mask <- r >= input$valueRange[1] & r <= input$valueRange[2]
      terra::ifel(mask, r, NA_real_)
    })

    # Render leaflet base map
    output$rasterMap <- renderLeaflet({
      leaflet(options = leafletOptions(zoomSnap = 0.05, zoomDelta = 0.05)) |>
        addTiles() |>
        setView(lng = 10, lat = 50, zoom = 4) |>
        addEasyButton(easyButton(
          icon = "fa-house",
          title = "Reset Zoom",
          onClick = JS("function(btn, map){ map.setView([50, 10], 4); }")
        )) |>
        addEasyButton(
          easyButton(
            icon = "fa-solid fa-fill-drip",
            title = "Transparency Control",
            onClick = JS(sprintf(
              "function(btn, map) {
      var panel = document.getElementById('%s');
      if (panel.style.display === 'none') {
        panel.style.display = 'block';
      } else {
        panel.style.display = 'none';
      }
    }",
              ns("opacityControl")
            ))
          )
        )
    })

    # Slider update
    observeEvent(raster_data(), {
      r <- raster_data()
      r_min <- min(r[], na.rm = TRUE)
      r_max <- max(r[], na.rm = TRUE)
      updateSliderInput(
        session,
        "valueRange",
        min = round(r_min, 4),
        max = round(r_max, 4),
        value = c(round(r_min, 4), round(r_max, 4))
      )
    })

    # explanation text for the climate model and climate scenario
    observeEvent(input$info_climate_models, {
      shinyalert(
        title = "Climate Model Information",
        html = TRUE,
        size = "m",
        type = "info",
        text = HTML(
          "<table style='width:100%; margin-bottom: 10px;'>
         <tr><th>Model</th><th>Institution</th></tr>
         <tr><td><b>mpi-esm1-2-hr</b></td><td>Max&nbsp;Planck&nbsp;Institute for Meteorology, Germany</td></tr>
         <tr><td><b>ipsl-cm6a-lr</b></td><td>Institut&nbsp;Pierre&nbsp;Simon&nbsp;Laplace, France</td></tr>
         <tr><td><b>ukesm1-0-ll</b></td><td>Met&nbsp;Office&nbsp;Hadley&nbsp;Centre, UK</td></tr>
         <tr><td><b>gfdl-esm4</b></td><td>NOAA&nbsp;GFDL, USA</td></tr>
         <tr><td><b>mri-esm2-0</b></td><td>Meteorological&nbsp;Research&nbsp;Institute, Japan</td></tr>
       </table>
       <p style='margin-top: 15px; font-weight: bold;'>
         <a href='https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf'
            target='_blank' style='text-decoration:none;'>
           📄 CHELSA V2.1 Technical Specifications
         </a>
       </p>"
        )
      )
    })

    observeEvent(input$info_climate_scenarios, {
      shinyalert(
        title = "Climate Scenario Information",
        html = TRUE,
        size = "m",
        type = "info",
        text = HTML(
          "<table style='width:100%; margin-bottom: 10px;'>
         <tr><th>Scenario</th><th>Description</th></tr>
         <tr><td><b>ssp126</b></td><td>SSP1-RCP2.6 climate as simulated by the GCMs.</td></tr>
         <tr><td><b>ssp370</b></td><td>SSP3-RCP7 climate as simulated by the GCMs.</td></tr>
         <tr><td><b>ssp585</b></td><td>SSP5-RCP8.5 climate as simulated by the GCMs.</td></tr>
       </table>
       <p style='margin-top: 15px;'>
         More information on the scenarios can be found
         <a href='https://www.dkrz.de/en/communication/climate-simulations/cmip6-en/the-ssp-scenarios'
            target='_blank' style='font-weight:bold; text-decoration:none;'>
           here
         </a>.
       </p>"
        )
      )
    })

    # Raster display
    observeEvent(filtered_raster(), {
      req(filtered_raster())

      r <- filtered_raster()
      full_range <- range(raster_data()[], na.rm = TRUE)
      pal <- leaflet::colorNumeric("plasma", domain = full_range, na.color = "transparent")

      legend_title <- get_projection_legend_title(
        input$dataTypePicker,
        input$showSpecies,
        input$speciesNamePicker
      )

      leafletProxy("rasterMap") %>%
        clearImages() %>%
        clearControls() %>%
        addRasterImage(
          r,
          colors = pal,
          opacity = input$opacitySlider,
          project = TRUE,
          layerId = "rasterLayer"
        ) %>%
        addLegend_decreasing(
          pal = pal,
          values = full_range,
          title = legend_title,
          position = "bottomright",
          decreasing = TRUE
        )
    })

    observeEvent(input$opacitySlider, {
      if (input$dataMode == "Projection" && !is.null(filtered_raster())) {
        r <- filtered_raster()
        full_range <- range(raster_data()[], na.rm = TRUE)
        pal <- leaflet::colorNumeric("plasma", domain = full_range, na.color = "transparent")

        legend_title <- get_projection_legend_title(
          input$dataTypePicker,
          input$showSpecies,
          input$speciesNamePicker
        )

        leafletProxy("rasterMap") %>%
          clearImages() %>%
          clearControls() %>%
          addRasterImage(
            r,
            colors = pal,
            opacity = input$opacitySlider,
            project = TRUE,
            layerId = "rasterLayer"
          ) %>%
          addLegend_decreasing(
            pal = pal,
            values = full_range,
            title = legend_title,
            position = "bottomright",
            decreasing = TRUE
          )
      } else if (input$dataMode == "Distribution" && !is.null(observed_raster())) {
        r <- observed_raster()
        full_range <- range(r[], na.rm = TRUE)

        if (input$showSpeciesDist && !is.null(input$speciesNamePickerDist)) {
          pal_cols <- c("#CCCCCC", "#2ca25f")

          leafletProxy("rasterMap", session = session) %>%
            clearImages() %>%
            clearControls() %>%
            addRasterImage(
              r,
              colors = pal_cols,
              opacity = input$opacitySlider,
              project = TRUE,
              layerId = "observedLayer"
            ) %>%
            addLegend(
              position = "bottomright",
              colors = pal_cols,
              labels = c("Absent", "Present"),
              title = "Presence / Absence",
              opacity = 1
            )
        } else {
          vir_pal <- leaflet::colorNumeric("viridis", domain = full_range, na.color = "transparent")

          leafletProxy("rasterMap", session = session) %>%
            clearImages() %>%
            clearControls() %>%
            addRasterImage(
              r,
              colors = vir_pal,
              opacity = input$opacitySlider,
              project = TRUE,
              layerId = "observedLayer"
            ) %>%
            addLegend_decreasing(
              pal = vir_pal,
              values = full_range,
              title = ifelse(input$obsType == "full", "Observed Distribution", "Modeled Distribution"),
              position = "bottomright",
              decreasing = TRUE
            )
        }
      }
    })

    observeEvent(input$loadObserved, {
      req(observed_raster())
      r <- observed_raster()
      full_range <- range(r[], na.rm = TRUE)

      # Palette & legend depend on whether we are in species mode
      if (input$showSpeciesDist && !is.null(input$speciesNamePickerDist)) {
        pal_cols <- c("#CCCCCC", "#2ca25f")

        leafletProxy("rasterMap", session = session) %>%
          clearImages() %>%
          clearControls() %>%
          addRasterImage(
            r,
            colors = pal_cols,
            opacity = input$opacitySlider,
            project = TRUE,
            layerId = "observedLayer"
          ) %>%
          addLegend(
            position = "bottomright",
            colors = pal_cols,
            labels = c("Absent", "Present"),
            title = "Presence / Absence",
            opacity = 1
          )
      } else {
        vir_pal <- leaflet::colorNumeric("viridis", domain = full_range, na.color = "transparent")

        leafletProxy("rasterMap", session = session) %>%
          clearImages() %>%
          clearControls() %>%
          addRasterImage(
            r,
            colors = vir_pal,
            opacity = input$opacitySlider,
            project = TRUE,
            layerId = "observedLayer"
          ) %>%
          addLegend_decreasing(
            pal = vir_pal,
            values = full_range,
            title = ifelse(input$obsType == "full", "Observed Distribution", "Modeled Distribution"),
            position = "bottomright",
            decreasing = TRUE
          )
      }
    })

    # Download handler
    output$downloadTif <- downloadHandler(
      filename = function() {
        if (input$dataMode == "Projection") {
          habitat <- names(habitat_mapping)[habitat_mapping == input$habitat]
          data_type <- switch(input$dataTypePicker, mean = "Mean", sd = "SD", cov = "Uncertainty", anomaly = "Anomaly")

          parts <- c("pDT-IAS", gsub(" ", "_", habitat), data_type, input$timeFramePicker)

          if (input$timeFramePicker == "Future") {
            parts <- c(parts, input$timePeriodPicker, input$climateModelPicker, input$climateScenarioPicker)
          }

          if (input$showSpecies && !is.null(input$speciesNamePicker)) {
            parts <- c(parts, gsub(" ", "_", input$speciesNamePicker))
          }
        } else {
          habitat <- names(habitat_mapping)[habitat_mapping == input$habitatDist]
          obs <- if (input$obsType == "full") "Observed" else "Modeled"

          parts <- c("pDT-IAS", gsub(" ", "_", habitat), obs)

          if (input$showSpeciesDist && !is.null(input$speciesNamePickerDist)) {
            parts <- c(parts, gsub(" ", "_", input$speciesNamePickerDist))
          }
        }

        paste0(paste(parts, collapse = "_"), ".tif")
      },

      content = function(file) {
        if (input$dataMode == "Projection") {
          row <- filtered_summary()
          if (nrow(row) == 0) {
            showNotification("No raster selected.", type = "error")
            return()
          }
          file_url <- row$tif_path[1]
        } else {
          row <- observed_summary()
          if (nrow(row) == 0) {
            showNotification("No raster selected.", type = "error")
            return()
          }
          file_url <- row$tif_path[1]
        }

        if (grepl("^https?://", file_url, ignore.case = TRUE)) {
          utils::download.file(file_url, destfile = file, mode = "wb", quiet = TRUE)
        } else {
          file.copy(from = file_url, to = file, overwrite = TRUE)
        }
      },

      contentType = "application/octet-stream"
    )

    output$selectedOptions <- renderUI({
      req(input$dataMode)

      if (input$dataMode == "Projection") {
        req(input$habitat, input$dataTypePicker, input$timeFramePicker)

        habitat <- names(habitat_mapping)[habitat_mapping == input$habitat]
        model_output <- switch(
          input$dataTypePicker,
          mean = "Mean",
          sd = "Standard Deviation",
          cov = "Coefficient of Variation",
          anomaly = "Prediction Anomaly"
        )

        details <- tagList(
          tags$strong("Habitat: "),
          habitat,
          tags$span(" | "),
          tags$strong("Model Output Type: "),
          model_output,
          tags$span(" | "),
          tags$strong("Time Frame: "),
          input$timeFramePicker
        )

        if (input$timeFramePicker == "Future") {
          req(input$timePeriodPicker, input$climateModelPicker, input$climateScenarioPicker)
          details <- tagList(
            details,
            tags$span(" | "),
            tags$strong("Time Period: "),
            input$timePeriodPicker,
            tags$span(" | "),
            tags$strong("Climate Model: "),
            input$climateModelPicker,
            tags$span(" | "),
            tags$strong("Climate Scenario: "),
            input$climateScenarioPicker
          )
        }

        if (input$showSpecies && !is.null(input$speciesNamePicker)) {
          details <- tagList(
            details,
            tags$span(" | "),
            tags$strong("Species: "),
            input$speciesNamePicker
          )
        }

        return(details)
      } else if (input$dataMode == "Distribution") {
        req(input$habitatDist, input$obsType)

        habitat <- names(habitat_mapping)[habitat_mapping == input$habitatDist]
        obs_type <- if (input$obsType == "full") "Observed Values" else "Modeled Values"

        details <- tagList(
          tags$strong("Habitat: "),
          habitat,
          tags$span(" | "),
          tags$strong("Observed Data Type: "),
          obs_type
        )

        if (input$showSpeciesDist && !is.null(input$speciesNamePickerDist)) {
          details <- tagList(
            details,
            tags$span(" | "),
            tags$strong("Species: "),
            input$speciesNamePickerDist
          )
        }

        return(details)
      }

      return(NULL)
    })

    observeEvent(input$rasterMap_click, {
      click <- input$rasterMap_click
      req(click$lng, click$lat)

      current_r <- if (input$dataMode == "Projection") raster_data() else observed_raster()
      req(!is.null(current_r))

      pt_wgs <- terra::vect(cbind(click$lng, click$lat), crs = "EPSG:4326")
      pt_proj <- terra::project(pt_wgs, terra::crs(current_r))

      val <- terra::extract(current_r, pt_proj)[1, 2]

      leafletProxy("rasterMap") %>%
        clearPopups() %>%
        addPopups(
          lng = click$lng,
          lat = click$lat,
          popup = paste0(
            "<div style='font-size:14px; line-height:1.5;'>",
            "<i class='fas fa-map-marker-alt' style='color:#FF5733;'></i> ",
            "<b>Longitude:</b> ",
            sprintf("%.2f", click$lng),
            "<br>",
            "<i class='fas fa-map-marker-alt' style='color:#FF5733;'></i> ",
            "<b>Latitude:</b> ",
            sprintf("%.2f", click$lat),
            "<br>",
            "<i class='fas fa-chart-line' style='color:#2ecc71;'></i> ",
            "<b>Value:</b> ",
            ifelse(is.na(val), "NA", round(val, 2)),
            "</div>"
          )
        )
    })
  })
}

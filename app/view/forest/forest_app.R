box::use(
  shiny,
  terra,
  leaflet,
  gridExtra,
  stringr,
  config,
  bslib,
  utils,
  shinyjs,
  echarty,
  echarty[ecs.output, ecs.render, ec.init],
)

box::use(
  app /
    logic /
    forest /
    analysis_functions[
      get_file_list,
      get_file_name,
      get_data,
      get_figure,
      get_multichart,
      get_experiment_data_file
    ],
)


#' @export
forest_app_ui <- function(id, i18n) {
  ns <- shiny$NS(id)
  shiny$tagList(
    bslib::layout_columns(
      col_widths = c(4, 8),
      bslib$card(
        shiny$tagList(
          shiny$selectInput(
            ns("management"),
            i18n$t("Select Forest Management Regime:"),
            choices = c("BAU", "EXT10", "EXT30", "GTR30", "NTLR", "NTSR", "SA")
          ),
          shiny$selectInput(
            ns("climate"),
            i18n$t("Select Climate Scenario:"),
            choices = c("Current climate", "RCP4.5", "RCP8.5")
          ),
          shiny$selectInput(
            ns("output"),
            i18n$t("Select Output Type:"),
            selectize = TRUE,
            # choices = c(
            #   "Above-ground biomass",
            #   "Below-ground biomass",
            #   "Harvested biomass",
            #   "Woody Debris",
            #   "Max-age of selected species",
            #   "Average age of all trees",
            #   "Median age of all trees"
            # )
            options = I(
              "
                  valueField: 'lowercase_shortcut',
                  labelField: 'label',
                  options: [
                    { lowercase_shortcut: 'above', label: ",
              i18n$t("Above-ground biomass"),
              " },
                    { lowercase_shortcut: 'below', label: ",
              i18n$t("Below-ground biomass"),
              " },
                    { lowercase_shortcut: 'harvested', label: ",
              i18n$t("Harvested biomass"),
              " },
                    { lowercase_shortcut: 'debris', label: ",
              i18n$t("Woody Debris"),
              " },
                    { lowercase_shortcut: 'maxage', label: ",
              i18n$t("Max-age of selected species"),
              " },
                    { lowercase_shortcut: 'avgage', label: ",
              i18n$t("Average age of all trees"),
              " },
                    { lowercase_shortcut: 'medianage', label",
              i18n$t("Median age of all trees"),
              " }
                  ]"
            )
          ),
          shiny$conditionalPanel(
            condition = sprintf(
              "input['%s'] == 'Above-ground biomass' || input['%s'] == 'Max-age of selected species'",
              ns("output"),
              ns("output")
            ),
            shiny$selectInput(
              ns("species"),
              i18n$t("Select Species Type:"),
              choices = c(
                "Birch (betulaSP)",
                "Pine (pinussyl)",
                "Spruce (piceabbies)",
                "Other trees (other)"
              )
            ),
          ),
          shiny$sliderInput(
            ns("res_file_slider"),
            i18n$t("Select year:"),
            min = 0,
            max = 0,
            value = 0
          ),
          shiny$checkboxInput(
            ns("show_results"),
            i18n$t("Show results"),
            value = FALSE
          )
        )
      ),
      bslib$card(
        shiny$textOutput("selection"),
        leaflet$leafletOutput(ns("map")),
        # echarty$ecs.output(
        #   ns("multichart"),
        #   # width = "100%",
        #   # height = "400px"
        # ),
      )
    ),
    bslib$card(
      echarty$ecs.output(
        ns("multichart"),
        # width = "100%",
        height = "400px"
      ),
      # leaflet$leafletOutput(ns("map")),
      # shiny$plotOutput(ns("plot"), height = "800px")
      # shiny$uiOutput(ns("plot"))
      echarty$ecs.output(
        ns("plot"),
        # width = "100%",
        height = "600px"
      )
    )
  )
}

#' @export
forest_app_server <- function(id, app_selected, i18n) {
  shiny$moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_folder <- file.path(config$get("data_path"), "forest_bird")
    output$selection <- shiny$renderText({
      text <- paste(
        "Management Regime:",
        input$management,
        "\n",
        "Climate Scenario:",
        input$climate,
        "\n",
        "Output Type:",
        input$output
      )

      if (input$output %in% c("Above-ground biomass", "Max-age of selected species")) {
        text <- paste(text, "\nSpecies Type:", input$species)
      }

      text
    })

    # shinInput & shinyRadiiobuttons dropdowns i18n solution, viz URL below ----
    # https://github.com/Appsilon/shiny.i18n/issues/54#issuecomment-751792229
    i18n_r <- shiny$reactive({
      i18n
    })
    shiny$observe({
      shiny$updateSelectInput(
        session,
        "output",
        choices = i18n_r()$t(
          c(
            "Above-ground biomass",
            "Below-ground biomass",
            "Harvested biomass",
            "Woody Debris",
            "Max-age of selected species",
            "Average age of all trees",
            "Median age of all trees"
          )
        )
      )
    })

    res_working_folder <- shiny$reactiveVal(NULL)
    res_file <- shiny$reactiveVal(NULL)
    experiment_data_file <- shiny$reactiveVal(NULL)

    output$map <- NULL

    shiny$observeEvent(
      app_selected(),
      ignoreInit = TRUE,
      {
        shiny$req(app_selected())
        output$map <- leaflet$renderLeaflet({
          leaflet$leaflet() |>
            leaflet$clearImages() |>
            leaflet$addTiles() |>
            leaflet$setView(
              lng = 24.545,
              lat = 60.192,
              zoom = 8
            )
        })
      }
    )

    # if there is a change in climate or management, only experiment_data_file is changed
    shiny$observeEvent(
      c(app_selected(), input$management, input$climate),
      ignoreInit = TRUE,
      {
        shiny$req(app_selected())

        experiment_data <- get_experiment_data_file(input, data_folder)

        experiment_data_file(experiment_data)
      }
    )

    # if experiment_data_file is changed update map and graph below and update res_file_name
    shiny$observeEvent(
      c(app_selected(), experiment_data_file()),
      ignoreInit = TRUE,
      {
        shiny$req(app_selected())

        experiment_data <- get_experiment_data_file(input, data_folder)

        input_selection <- get_file_list(
          input,
          data_folder,
          experiment_data
        )

        experiment_data_file(experiment_data)
        res_working_folder(input_selection$res_working_folder)
        res_file_list_tick <- input_selection$res_file_list_tick
        timestep <- input_selection$timestep
        start_year <- input_selection$start_year
        # duration <- input_selection$duration
        simulated_years <- res_file_list_tick + start_year

        if (length(simulated_years) > 0) {
          min_value <- min(simulated_years, na.rm = TRUE)
          max_value <- max(simulated_years, na.rm = TRUE)

          # Get the current value of the slider
          value <- shiny$isolate(input$res_file_slider)

          # Ensure the value stays within the new range
          if (value < min_value) {
            value <- min_value
          } else if (value > max_value) {
            value <- max_value
          }

          shiny$updateSliderInput(
            session,
            "res_file_slider",
            min = min_value,
            max = max_value,
            value = value,
            step = timestep
          )

          shinyjs$delay(
            100,
            {
              # Update res_file based on the slider value (file names starts from 0)
              res_file_name <- get_file_name(input, input_selection$res_folder, value - start_year)
              res_file(res_file_name)
            }
          )
        }
      }
    )

    # if output, species or res_file_slider is changed, update map and res_file_name
    shiny$observeEvent(
      c(app_selected(), experiment_data_file(), input$output, input$species, input$res_file_slider),
      ignoreInit = TRUE,
      {
        shiny$req(app_selected())

        # experiment_data <- get_experiment_data_file(input, data_folder)
        input_selection <- get_file_list(
          input,
          data_folder,
          experiment_data_file()
        )

        # experiment_data_file(experiment_data)
        res_working_folder(input_selection$res_working_folder)
        res_file_list_tick <- input_selection$res_file_list_tick
        timestep <- input_selection$timestep
        start_year <- input_selection$start_year
        # duration <- input_selection$duration
        simulated_years <- res_file_list_tick + start_year

        if (length(simulated_years) > 0) {
          min_value <- min(simulated_years, na.rm = TRUE)
          max_value <- max(simulated_years, na.rm = TRUE)

          # Get the current value of the slider
          value <- shiny$isolate(input$res_file_slider)

          # Ensure the value stays within the new range
          if (value < min_value) {
            value <- min_value
          } else if (value > max_value) {
            value <- max_value
          }

          shiny$updateSliderInput(
            session,
            "res_file_slider",
            min = min_value,
            max = max_value,
            value = value,
            step = timestep
          )

          shinyjs$delay(
            100,
            {
              # Update res_file based on the slider value (file names starts from 0)
              res_file_name <- get_file_name(input, input_selection$res_folder, value - start_year)
              res_file(res_file_name)
            }
          )
        }
      }
    )

    # Map update ----
    shiny$observeEvent(
      c(
        res_file()
      ),
      ignoreInit = TRUE,
      {
        shiny$req(res_file())
        # shiny$req(experiment_data_file())

        raster_data <- terra$rast(
          file.path(data_folder, res_file())
        )
        # raster_data <- terra$aggregate(raster_data, fact = 2, fun = mean)

        ext <- terra$ext(raster_data)

        terra$values(raster_data) |> max(na.rm = TRUE) |> is.infinite() |> print()
        if (terra$values(raster_data) |> max(na.rm = TRUE) |> is.infinite()) {
          shiny$showNotification("Warning: Raster contains infinite values!", type = "error")
        }

        pal <- leaflet$colorNumeric(
          palette = "YlOrBr",
          # domain = terra$values(raster_data),
          domain = terra$values(raster_data)[is.finite(terra$values(raster_data))],
          na.color = "transparent",
          reverse = TRUE
        )
        # raster_data <- terra$aggregate(raster_data, fact = 2, fun = mean)

        leaflet$leafletProxy("map") |>
          leaflet$clearImages() |>
          leaflet$clearControls() |>
          leaflet$addRasterImage(
            raster_data,
            opacity = 0.4,
            colors = pal,
            project = FALSE
          ) |>
          leaflet$addLegend(
            position = "bottomright",
            pal = leaflet$colorNumeric(
              palette = "YlOrBr",
              # domain = terra$values(raster_data),
              domain = terra$values(raster_data)[is.finite(terra$values(raster_data))],
              na.color = "transparent"
            ),
            values = terra$values(raster_data),
            opacity = 0.4
          )
      }
    )

    # graph update
    shiny$observeEvent(
      c(
        experiment_data_file()
      ),
      ignoreInit = TRUE,
      {
        chart <- get_multichart(experiment_data_file())
        experiment_chart(chart)
        output$multichart <- echarty$ecs.render(
          experiment_chart()
        )
      }
    )

    output_plot <- shiny$reactiveVal(NULL)

    shiny$observeEvent(
      c(app_selected(), input$show_results),
      {
        plots <- list()

        if (input$show_results) {
          climate_scenarios <- c("current", "4.5", "8.5")
          management_scenarios <- c("BAU", "EXT10", "EXT30", "GTR30", "NTLR", "NTSR", "SA")
          years <- seq(0, 100, by = 10)

          combined_data <- get_data(climate_scenarios, management_scenarios, years, data_folder)
          plots <- get_figure(combined_data)
        }

        output_plot(plots)
      }
    )

    output$plot <- echarty$ecs.render(
      output_plot()
    )

    experiment_chart <- shiny$reactiveVal(NULL)

    shiny$observeEvent(
      app_selected(),
      {
        # Create a basic bar chart
        chart <- echarty$ec.init()
        experiment_chart(chart)
      }
    )

    output$multichart <- echarty$ecs.render(
      experiment_chart()
    )
  })
}

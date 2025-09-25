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
      get_experiment_data_file,
      get_bird_species_list
    ],
  app /
    logic /
    forest /
    plot_helper_functions[
      plot_bird_species,
      plot_tree_species,
      get_multichart,
      get_figure
    ],
  app /
    logic /
    forest /
    landis_io[
      read_landis_params
    ],
  app / logic / translate_multiple_choices[translate_multiple_choices],
)

climate_scenarios <- c("Current climate", "RCP4.5", "RCP8.5")

output_types <- c(
  "Above-ground biomass",
  "Below-ground biomass",
  "Harvested biomass",
  "Woody Debris",
  "Max-age of selected species",
  "Average age of all trees",
  "Median age of all trees"
)

tree_species <- c(
  "Birch (betulaSP)",
  "Pine (pinussyl)",
  "Spruce (piceabbies)",
  "Other trees (other)"
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
            choices = climate_scenarios
          ),
          shiny$selectInput(
            ns("output"),
            i18n$t("Select Output Type:"),
            choices = output_types
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
              choices = tree_species
            ),
          ),
          shiny$selectInput(
            ns("bird_species"),
            label = i18n$t("Select Bird Species:"),
            choices = c(
              i18n$t("None")
            )
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
      )
    ),
    bslib$card(
      echarty$ecs.output(
        ns("multichart"),
        height = "400px",
      ),
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
    prediction_folder <- file.path(data_folder, "predictions")

    output$selection <- shiny$renderText({
      text <- paste(
        i18n$t("Management Regime:"),
        input$management,
        "\n",
        i18n$t("Climate Scenario:"),
        input$climate,
        "\n",
        i18n$t("Output Type:"),
        input$output
      )

      if (input$output %in% c("Above-ground biomass", "Max-age of selected species")) {
        text <- paste(text, "\nSpecies Type:", input$species)
      }

      text
    })

    shiny$observe({
      translate_multiple_choices(
        session,
        which_type = "select",
        input_id = "climate",
        label = "Select Climate Scenario:",
        inline = FALSE,
        i18n,
        choices_type = "singlelist",
        selected_choice = input$climate,
        climate_scenarios
      )
    })

    shiny$observe({
      translate_multiple_choices(
        session,
        which_type = "select",
        input_id = "output",
        label = "Select Output Type:",
        inline = FALSE,
        i18n,
        choices_type = "singlelist",
        selected_choice = input$output,
        output_types
      )
    })

    shiny$observe({
      translate_multiple_choices(
        session,
        which_type = "select",
        input_id = "species",
        label = "Select Species Type:",
        inline = FALSE,
        i18n,
        choices_type = "singlelist",
        selected_choice = input$species,
        tree_species
      )
    })

    res_working_folder <- shiny$reactiveVal(NULL)
    res_file <- shiny$reactiveVal(NULL)
    experiment_data_file <- shiny$reactiveVal(NULL)
    start_sim_year <- shiny$reactiveVal(NULL)
    # remember last valid selections to revert on invalid input combos
    last_ok <- shiny$reactiveValues(
      management = NULL,
      climate = NULL,
      output = NULL,
      species = NULL,
      res_file_slider = NULL,
      experiment_data_file = NULL
    )

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
            leaflet$addLayersControl(
              overlayGroups = c("tree_species", "bird_species"),
              options = leaflet$layersControlOptions(collapsed = FALSE)
            ) |>
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

        experiment_data <- get_experiment_data_file(input, data_folder = data_folder)

        experiment_data_file(experiment_data)

        bird_species_list <- get_bird_species_list(basename(experiment_data_file()), prediction_folder)

        shiny$updateSelectInput(
          session,
          "bird_species",
          choices = c(
            i18n$t("None"),
            bird_species_list
          )
        )
      }
    )

    # if experiment_data_file is changed update map and graph below and update res_file_name
    shiny$observeEvent(
      c(app_selected(), experiment_data_file()),
      ignoreInit = TRUE,
      {
        shiny$req(app_selected())
        experiment_data <- get_experiment_data_file(input, data_folder = data_folder)

        input_selection <- get_file_list(
          input,
          data_folder,
          experiment_data, # "/home/osalamon/WORK/biodt-shiny/app/data/forest_bird/run_landis_current_BAU_7141504"
          i18n
        )
        # If selection failed, revert management/climate to last valid (keep current raster visible)
        if (is.null(input_selection)) {
          # revert only if we have a stored valid state
          if (!is.null(last_ok$management) && !is.null(last_ok$climate)) {
            if (!identical(input$management, last_ok$management)) {
              shiny$updateSelectInput(session, "management", selected = last_ok$management)
            }
            if (!identical(input$climate, last_ok$climate)) {
              shiny$updateSelectInput(session, "climate", selected = last_ok$climate)
            }
            shiny$showNotification(i18n$t("Invalid combination. Reverted to last valid selection."), type = "warning")
          }
          return(invisible(NULL))
        }

        experiment_data_file(experiment_data)
        res_working_folder(input_selection$res_working_folder)
        res_file_list_tick <- input_selection$res_file_list_tick
        timestep <- input_selection$timestep
        start_year <- input_selection$start_year
        # duration <- input_selection$duration
        start_sim_year(start_year)
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

        #  shinyjs$delay(
        #    100,
        #    {
              # Update res_file based on the slider value (file names starts from 0)
              res_file_name <- get_file_name(
                input,
                input_selection$res_folder,
                value - start_year,
                i18n
              )
              res_file(res_file_name)
              # store last valid state for revert baseline
              last_ok$management <- input$management
              last_ok$climate <- input$climate
              last_ok$output <- input$output
              last_ok$species <- input$species
              last_ok$res_file_slider <- value
              last_ok$experiment_data_file <- experiment_data
        #    }
        #  )
        }
      }
    )

    # if output, species or res_file_slider is changed, update map and res_file_name
    shiny$observeEvent(
      c(app_selected(), experiment_data_file(), input$output, input$species, input$res_file_slider),
      ignoreInit = TRUE,
      {
        shiny$req(app_selected())

        experiment_data <- get_experiment_data_file(input, data_folder = data_folder)

        input_selection <- get_file_list(
          input,
          data_folder,
          # OK "/home/osalamon/WORK/biodt-shiny/app/data/forest_bird"
          experiment_data_file(),
          # OK "/home/osalamon/WORK/biodt-shiny/app/data/forest_bird/run_landis_current_BAU_7141504",
          i18n
        )
        # If selection failed, revert the changed input(s) (keep current raster visible)
        if (is.null(input_selection)) {
          # Determine which of output/species/slider changed vs last_ok and revert
          if (!is.null(last_ok$output) && !identical(input$output, last_ok$output)) {
            shiny$updateSelectInput(session, "output", selected = last_ok$output)
          }
          if (!is.null(last_ok$species) && !identical(input$species, last_ok$species)) {
            shiny$updateSelectInput(session, "species", selected = last_ok$species)
          }
          if (!is.null(last_ok$res_file_slider) && !identical(input$res_file_slider, last_ok$res_file_slider)) {
            shiny$updateSliderInput(session, "res_file_slider", value = last_ok$res_file_slider)
          }
          shiny$showNotification(i18n$t("Invalid selection. Reverted to last valid option."), type = "warning")
          return(invisible(NULL))
        }

        # experiment_data_file(experiment_data)
        res_working_folder(input_selection$res_working_folder)
        res_file_list_tick <- input_selection$res_file_list_tick
        timestep <- input_selection$timestep
        start_year <- input_selection$start_year
        # duration <- input_selection$duration
        simulated_years <- start_year + res_file_list_tick

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

        #  shinyjs$delay(
        #    100,
        #    {
              # Update res_file based on the slider value (file names starts from 0)
              res_file_name <- get_file_name(
                input,
                input_selection$res_folder,
                value - start_year,
                i18n
              )
              res_file(res_file_name)
              # store last valid state
              last_ok$management <- input$management
              last_ok$climate <- input$climate
              last_ok$output <- input$output
              last_ok$species <- input$species
              last_ok$res_file_slider <- value
              last_ok$experiment_data_file <- experiment_data
      #      }
      #    )
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

        plot_tree_species(data_folder, res_file(), i18n)
      }
    )

    # graph update
    shiny$observeEvent(
      c(
        experiment_data_file()
      ),
      ignoreInit = TRUE,
      {
        # Validate experiment data directory before building chart
        if (is.null(experiment_data_file()) || length(experiment_data_file()) == 0 ||
          !dir.exists(experiment_data_file())) {
          shiny$showNotification("Experiment data directory not found or invalid.", type = "error")
          return(invisible(NULL))
        }
        chart <- get_multichart(experiment_data_file(), i18n)
        experiment_chart(chart)
        output$multichart <- echarty$ecs.render(
          experiment_chart()
        )
      }
    )


    # bird species update
    shiny$observeEvent(
      c(
        input$res_file_slider, input$bird_species
      ),
      ignoreInit = TRUE,
      {
        if (is.null(input$bird_species) || identical(input$bird_species, "None")) {
          leaflet$leafletProxy("map") |>
            leaflet$removeImage("bird_species")
          return()
        }
        plot_bird_species(
          scenario = basename(experiment_data_file()),
          bird_species = input$bird_species,
          tick = input$res_file_slider - start_sim_year(),
          prediction_folder = prediction_folder,
          i18n
        )
      }
    )

    output_plot <- shiny$reactiveVal(NULL)

    shiny$observeEvent(
      c(app_selected(), input$show_results),
      {
        plots <- list()

        if (input$show_results) {
          params <- read_landis_params(experiment_data_file())

          climate_scenarios <- c("current", "4.5", "8.5")
          management_scenarios <- c("BAU", "EXT10", "EXT30", "GTR30", "NTLR", "NTSR", "SA")
          years <- seq(0, params$duration, by = params$timestep)

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

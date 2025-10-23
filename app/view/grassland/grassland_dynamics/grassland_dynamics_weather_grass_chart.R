box::use(
  shiny[NS, moduleServer, tags, reactive, observeEvent, selectInput, actionButton, updateSelectInput, req, reactiveVal],
  bslib[card, card_header, card_body, layout_column_wrap],
  echarty[ecs.output, ecs.render],
  waiter[Waiter],
  config,
  stats[setNames],
)

box::use(
  app / logic / grassland / grassland_multichart_lines[generate_chart_lines],
  app / logic / grassland / grassland_multichart_bars_stack[generate_chart_bars_mean],
  app / logic / grassland / grassland_multichart_lines_mean[generate_chart_lines_mean],
  app / logic / grassland / grassland_load_simulation_data[load_grassland_simulation_data],
  app / logic / waiter[waiter_text],
)

#' @export
grassland_dynamics_double_chart_ui <- function(
  id,
  i18n,
  plot_width = "100%",
  plot_height = "1000px"
) {
  ns <- NS(id)
  card(
    id = ns("multichart_wrap"),
    class = "mx-md-3 card-shadow mb-2",
    full_screen = TRUE,
    card_header(
      tags$h2(
        i18n$translate("Grassland Simulations and Weather"),
        class = "card_title"
      )
    ),
    card_body(
      style = "min-width: 900px !important; overflow-x: scroll;",
      layout_column_wrap(
        width = 1 / 2,
        selectInput(
          ns("output_list"),
          label = i18n$translate("Choose output dataset"),
          choices = c("Default (project1)" = "default")
        ),
        actionButton(
          ns("update_output"),
          label = i18n$translate("Show results"),
          class = "mt-auto"
        )
      ),
      ecs.output(
        ns("multichart"),
        width = plot_width,
        height = plot_height
      )
    )
  )
}

#' @export
grassland_dynamics_double_chart_server <- function(
  id,
  plot_type,
  available_runs,
  current_simulation_data,
  tab_grassland_selected,
  i18n
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Define waiter ----
    msg <- waiter_text(message = tags$h3(i18n$t("Loading..."), style = "color: #414f2f;"))
    w <- Waiter$new(
      id = ns("multichart_wrap"),
      html = msg,
      color = "rgba(256,256,256,0.9)",
    )

    # Update dropdown choices when available_runs changes ----
    observeEvent(
      available_runs(),
      priority = 1000,
      {
        runs <- available_runs()

        if (length(runs) > 0) {
          # Build choices list with default + simulation runs
          choices <- c("Default (project1)" = "default")
          for (run_id in names(runs)) {
            run_meta <- runs[[run_id]]
            choices <- c(choices, setNames(run_id, run_meta$label))
          }

          # Store the most recent run ID for auto-load
          most_recent_run <- names(runs)[length(runs)]

          # Update selectInput choices
          updateSelectInput(
            session,
            "output_list",
            choices = choices,
            selected = most_recent_run # Select most recent run
          )
        }
      }
    )

    update_data <- reactive({
      input$update_output
      input$output_list
    })

    # Load data when button is clicked OR when auto_load_trigger changes ----
    observeEvent(
      {
        update_data()
      },
      ignoreInit = TRUE, # Don't trigger on initial reactive setup
      {
        req(input$output_list)
        w$show()

        selected_run <- input$output_list

        if (selected_run == "default") {
          # Load default "Example" data using unified loader
          # Base directory is app/data/grassland/ with scenarios/ and simulations/ subdirs
          default_data <- load_grassland_simulation_data(
            run_dir = file.path(config$get("data_path"), "grassland"),
            lat = 51.3919,
            lon = 11.8787,
            start_year = 2013,
            end_year = 2015,
            run_number = 0
          )

          # Check if data loaded successfully
          if (is.null(default_data$grass_output) || is.null(default_data$weather_data)) {
            w$hide()
            return()
          }

          # Update shared simulation data for data tables
          current_simulation_data(default_data)

          # Generate charts using original functions with file paths
          colors_for_grass <- c("#18A547", "#AF2C6E", "#422CAF")
          colors_for_grass_lighter <- c("#73eb9b", "#e28bb7", "#998be2")
          colors_for_weather <- c("#0072B2", "#ae4d18", "#956618", "#108039", "#56B4E9")

          chart_reactive <- reactive({
            if (plot_type() == "bar") {
              generate_chart_bars_mean(
                filepaths_grass = default_data$metadata$grass_files,
                filepath_weather = default_data$metadata$weather_file,
                colors_for_grass = colors_for_grass,
                colors_for_weather = colors_for_weather,
                grass_end_date = default_data$metadata$grass_end_date,
                i18n = i18n
              )
            } else if (plot_type() == "line") {
              generate_chart_lines(
                filepaths_grass = default_data$metadata$grass_files,
                filepath_weather = default_data$metadata$weather_file,
                colors_for_grass = colors_for_grass_lighter,
                colors_for_weather = colors_for_weather,
                grass_end_date = default_data$metadata$grass_end_date,
                i18n = i18n
              )
            } else if (plot_type() == "line_mean") {
              generate_chart_lines_mean(
                filepaths_grass = default_data$metadata$grass_files,
                filepath_weather = default_data$metadata$weather_file,
                colors_for_grass = colors_for_grass,
                colors_for_weather = colors_for_weather,
                grass_end_date = default_data$metadata$grass_end_date,
                i18n = i18n
              )
            }
          })

          output$multichart <- ecs.render(chart_reactive())
        } else {
          # Load simulation run data using unified loader
          runs <- available_runs()
          run_meta <- runs[[selected_run]]

          req(run_meta)

          # Load all simulation data
          sim_data <- load_grassland_simulation_data(
            run_dir = run_meta$run_dir,
            lat = run_meta$lat,
            lon = run_meta$lon,
            start_year = run_meta$start_year,
            end_year = run_meta$end_year,
            run_number = run_meta$run_number
          )

          # Check if data loaded successfully
          if (is.null(sim_data$grass_output) || is.null(sim_data$weather_data)) {
            w$hide()
            return()
          }

          # Update shared simulation data for data tables
          current_simulation_data(sim_data)

          # Generate chart using original functions with file paths
          colors_for_grass <- c("#18A547", "#AF2C6E", "#422CAF")
          colors_for_grass_lighter <- c("#73eb9b", "#e28bb7", "#998be2")
          colors_for_weather <- c("#0072B2", "#ae4d18", "#956618", "#108039", "#56B4E9")

          chart_reactive <- reactive({
            if (plot_type() == "bar") {
              generate_chart_bars_mean(
                filepaths_grass = sim_data$metadata$grass_files,
                filepath_weather = sim_data$metadata$weather_file,
                colors_for_grass = colors_for_grass,
                colors_for_weather = colors_for_weather,
                grass_end_date = sim_data$metadata$grass_end_date,
                i18n = i18n
              )
            } else if (plot_type() == "line") {
              generate_chart_lines(
                filepaths_grass = sim_data$metadata$grass_files,
                filepath_weather = sim_data$metadata$weather_file,
                colors_for_grass = colors_for_grass_lighter,
                colors_for_weather = colors_for_weather,
                grass_end_date = sim_data$metadata$grass_end_date,
                i18n = i18n
              )
            } else if (plot_type() == "line_mean") {
              generate_chart_lines_mean(
                filepaths_grass = sim_data$metadata$grass_files,
                filepath_weather = sim_data$metadata$weather_file,
                colors_for_grass = colors_for_grass,
                colors_for_weather = colors_for_weather,
                grass_end_date = sim_data$metadata$grass_end_date,
                i18n = i18n
              )
            }
          })

          output$multichart <- ecs.render(chart_reactive())
        }

        w$hide()
      }
    )

    # Initial load on tab selection ----
    observeEvent(
      tab_grassland_selected(),
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      {
        # Trigger initial load with default data
        if (input$output_list == "default") {
          w$show()

          # Load default "Example" data using unified loader
          default_data <- load_grassland_simulation_data(
            run_dir = file.path(config$get("data_path"), "grassland"),
            lat = 51.3919,
            lon = 11.8787,
            start_year = 2013,
            end_year = 2015,
            run_number = 0
          )

          # Update shared simulation data
          current_simulation_data(default_data)

          colors_for_grass <- c("#18A547", "#AF2C6E", "#422CAF")
          colors_for_grass_lighter <- c("#73eb9b", "#e28bb7", "#998be2")
          colors_for_weather <- c("#0072B2", "#ae4d18", "#956618", "#108039", "#56B4E9")

          chart_reactive <- reactive({
            if (plot_type() == "bar") {
              generate_chart_bars_mean(
                filepaths_grass = default_data$metadata$grass_files,
                filepath_weather = default_data$metadata$weather_file,
                colors_for_grass = colors_for_grass,
                colors_for_weather = colors_for_weather,
                grass_end_date = default_data$metadata$grass_end_date,
                i18n = i18n
              )
            } else if (plot_type() == "line") {
              generate_chart_lines(
                filepaths_grass = default_data$metadata$grass_files,
                filepath_weather = default_data$metadata$weather_file,
                colors_for_grass = colors_for_grass_lighter,
                colors_for_weather = colors_for_weather,
                grass_end_date = default_data$metadata$grass_end_date,
                i18n = i18n
              )
            } else if (plot_type() == "line_mean") {
              generate_chart_lines_mean(
                filepaths_grass = default_data$metadata$grass_files,
                filepath_weather = default_data$metadata$weather_file,
                colors_for_grass = colors_for_grass,
                colors_for_weather = colors_for_weather,
                grass_end_date = default_data$metadata$grass_end_date,
                i18n = i18n
              )
            }
          })

          output$multichart <- ecs.render(
            chart_reactive()
          )
          w$hide()
        }
      }
    )
  })
}

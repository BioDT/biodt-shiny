box::use(
  shiny[
    NS,
    moduleServer,
    tags,
    reactive,
    observeEvent,
    selectInput,
    updateSelectInput,
    req,
    reactiveVal,
    downloadHandler,
    downloadButton,
    showNotification,
    icon
  ],
  bslib[card, card_header, card_body, layout_column_wrap],
  echarty[ecs.output, ecs.render],
  waiter[Waiter],
  config,
  stats[setNames],
  zip[zip],
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
          choices = c("Default (Halle, Germany 2018-2024)" = "default")
        ),
        downloadButton(
          ns("download_results"),
          label = i18n$translate("Download results"),
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
          # Build choices list from all simulation runs
          choices <- c()
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

    # Load data when dropdown selection changes ----
    observeEvent(
      input$output_list,
      ignoreInit = TRUE, # Don't trigger on initial reactive setup
      {
        req(input$output_list)
        w$show()

        selected_run <- input$output_list

        # Get run metadata
        runs <- available_runs()
        run_meta <- runs[[selected_run]]

        req(run_meta)

        # Load all simulation data using unified loader
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

        # Define chart colors
        colors_for_grass <- c("#18A547", "#AF2C6E", "#422CAF")
        colors_for_grass_lighter <- c("#73eb9b", "#e28bb7", "#998be2")
        colors_for_weather <- c("#0072B2", "#ae4d18", "#956618", "#108039", "#56B4E9")

        # Generate chart based on plot type
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
        w$hide()
      }
    )

    # Download handler for results ----
    output$download_results <- downloadHandler(
      filename = function() {
        selected_run <- input$output_list
        runs <- available_runs()
        run_meta <- runs[[selected_run]]
        paste0("grassland_run_", run_meta$run_number, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        # Show waiter while preparing download
        w$show()

        tryCatch(
          {
            selected_run <- input$output_list
            runs <- available_runs()
            run_meta <- runs[[selected_run]]
            dir_to_zip <- run_meta$run_dir

            # Create zip file
            files_to_zip <- list.files(dir_to_zip, recursive = TRUE)

            if (length(files_to_zip) > 0) {
              zip::zip(
                zipfile = file,
                files = files_to_zip,
                root = dir_to_zip
              )
            } else {
              # Show error notification if no files found
              showNotification(
                ui = tags$div(
                  tags$strong(i18n$t("Error: No files found")),
                  tags$br(),
                  i18n$t("Please report this issue on "),
                  tags$a(
                    "Github",
                    icon("github"),
                    href = "https://github.com/BioDT/biodt-shiny/issues",
                    target = "_blank"
                  ),
                  "."
                ),
                type = "error",
                duration = 10
              )
              # Create empty zip to prevent download error
              file.create(file)
            }
          },
          finally = {
            w$hide()
          }
        )
      }
    )

    # Initial load on tab selection ----
    observeEvent(
      tab_grassland_selected(),
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      {
        # Trigger initial load with currently selected data
        req(input$output_list)
        w$show()

        selected_run <- input$output_list
        runs <- available_runs()
        run_meta <- runs[[selected_run]]

        req(run_meta)

        # Load simulation data
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

        # Update shared simulation data
        current_simulation_data(sim_data)

        # Define chart colors
        colors_for_grass <- c("#18A547", "#AF2C6E", "#422CAF")
        colors_for_grass_lighter <- c("#73eb9b", "#e28bb7", "#998be2")
        colors_for_weather <- c("#0072B2", "#ae4d18", "#956618", "#108039", "#56B4E9")

        # Generate chart based on plot type
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
        w$hide()
      }
    )
  })
}

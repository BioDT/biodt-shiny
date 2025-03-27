box::use(
  shiny[NS, moduleServer, tags, reactive, observeEvent, selectInput, actionButton],
  bslib[card, card_header, card_body, layout_column_wrap],
  echarty[ecs.output, ecs.render],
  waiter[Waiter],
  shinyjs[disabled],
  config,
)

box::use(
  app / logic / grassland / grassland_multichart_lines[generate_chart_lines],
  app / logic / grassland / grassland_multichart_bars_stack[generate_chart_bars_mean],
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
        class = "card_title",
        i18n$translate("Grassland Simulations & Weather")
      )
    ),
    card_body(
      layout_column_wrap(
        width = 1 / 2,
        selectInput(
          ns("output_list"),
          label = i18n$translate("Choose output dataset"),
          choices = NULL
        ),
        disabled(
          actionButton(
            ns("update_output"),
            label = i18n$translate("Show results"),
            class = "mt-auto"
          )
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
grassland_dynamics_double_chart_server <- function(id, plot_type, mean_switch, tab_grassland_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Define waiter ----
    msg <- waiter_text(message = tags$h3("Loading...", style = "color: #414f2f;"))
    w <- Waiter$new(
      id = ns("multichart_wrap"),
      html = msg,
      color = "rgba(256,256,256,0.9)",
    )

    observeEvent(
      tab_grassland_selected(),
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      {
        w$show()

        files_grass <- list.files(
          file.path(config$get("data_path"), "grassland", "simulations", "project1", "output"),
          full.names = TRUE
        )
        file_weather <- file.path(
          config$get("data_path"),
          "grassland",
          "scenarios",
          "lat51.391900_lon11.878700",
          "weather",
          "lat51.391900_lon11.878700__2013-01-01_2023-12-31__weather.txt"
        )

        colors_for_grass <- c("#18A547", "#AF2C6E", "#422CAF")
        colors_for_grass_lighter <- c("#73eb9b", "#e28bb7", "#998be2")
        colors_for_weather <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00") #colors_for_weather <- c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF")
        end_date <- "2015-12-31"

        chart_reactive <- reactive({
          if (plot_type() == "bar") {
            generate_chart_bars_mean(
              filepaths_grass = files_grass,
              filepath_weather = file_weather,
              colors_for_grass = colors_for_grass,
              colors_for_weather = colors_for_weather,
              grass_end_date = end_date
            )
          } else if (plot_type() == "line") {
            generate_chart_lines(
              filepaths_grass = files_grass,
              filepath_weather = file_weather,
              colors_for_grass = colors_for_grass_lighter,
              colors_for_weather = colors_for_weather,
              grass_end_date = end_date
            )
          }
        })

        output$multichart <- ecs.render(
          chart_reactive()
        )
        w$hide()
      }
    )
  })
}

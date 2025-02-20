box::use(
  shiny[NS, moduleServer, tags, reactive],
  bslib[card, card_header, card_body],
  echarty[ecs.output, ecs.render],
  waiter[Waiter],
)

box::use(
  app / logic / grassland / grassland_weather_double_chart[generate_chart_with_weather],
  app / logic / waiter[waiter_text],
)

#' @export
grassland_dynamics_double_chart_ui <- function(
    id,
    i18n,
    plot_width = "100%",
    plot_height = "600px") {
  ns <- NS(id)
  card(
    id = ns("double_chart_wrap"),
    class = "mx-md-3 card-shadow mb-2",
    full_screen = TRUE,
    card_header(
      tags$h2(
        class = "card_title",
        i18n$translate("Grassland Simulations & Weather")
      )
    ),
    card_body(
      ecs.output(
        ns("double_chart"),
        width = plot_width,
        height = plot_height
      )
    )
  )
}

#' @export
grassland_dynamics_double_chart_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Define waiter ----
    msg <- list(
      waiter_text(message = tags$h3("Loading chart...",
        style = "color: #414f2f;"
      ))
    )
    w <- Waiter$new(
      id = ns("double_chart_wrap"),
      html = msg[[1]],
      color = "rgba(256,256,256,0.9)",
    )

    files_grass <- list.files("app/data/grassland/simulations/project1/output", full.names = TRUE)
    file_weather <- "app/data/grassland/scenarios/lat51.391900_lon11.878700/weather/lat51.391900_lon11.878700__2013-01-01_2023-12-31__weather.txt"
    colors_for_grass <- c("#b4e4b4", "#dfa7a7", "#9c9cdf")
    colors_for_weather <- c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF")
    end_date <- "2015-12-31"

    chart_reactive <- reactive({
      w$show()
      on.exit({
        w$hide()
      })

      generate_chart_with_weather(
        filepaths_grass = files_grass,
        filepath_weather = file_weather,
        colors_for_grass = colors_for_grass,
        colors_for_weather = colors_for_weather,
        grass_end_date = end_date
      )
    })

    output$double_chart <- ecs.render(
      chart_reactive()
    )

  })
}
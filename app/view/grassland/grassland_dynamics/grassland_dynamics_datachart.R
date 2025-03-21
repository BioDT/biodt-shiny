box::use(
  shiny[NS, moduleServer, tags, reactive],
  bslib[card, card_header, card_body],
  echarty[ecs.output, ecs.render],
  waiter[Waiter],
  config,
)

box::use(
  app / logic / grassland / grassland_echart_modular[generate_chart],
  app / logic / waiter[waiter_text],
)

#' @export
grassland_dynamics_datachart_ui <- function(
  id,
  i18n,
  plot_width = "100%",
  plot_height = "500px"
) {
  ns <- NS(id)
  card(
    id = ns("datachart"),
    class = "ms-md-3 card-shadow mb-2",
    full_screen = TRUE,
    card_header(
      tags$h2(
        class = "card_title",
        i18n$translate("PFTs Chart")
      )
    ),
    card_body(
      ecs.output(
        ns("pft_chart"),
        width = plot_width,
        height = plot_height
      )
    )
  )
}

#' @export
grassland_dynamics_datachart_server <- function(id, plot_type, mean_switch) {
  # nolint
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Define waiter ----
    msg <- waiter_text(message = tags$h3("Loading chart...", style = "color: #414f2f;"))
    w <- Waiter$new(
      id = ns("datachart"),
      html = msg,
      color = "rgba(256,256,256,0.9)",
    )

    # Parameters for chart ----
    filepaths_results <- list.files(
      file.path(config$get("data_path"), "grassland", "simulations", "project1", "output"),
      full.names = TRUE
    )
    stack <- "total"
    series_opacity <- 0.01
    colors <- c("#00ab4a", "#ae0000", "#003fc8")
    colors_series <- c("#b4e4b4", "#dfa7a7", "#9c9cdf")
    colors_series <- c("#b4e4b4", "#dfa7a7", "#9c9cdf")

    chart_reactive <- reactive({
      w$show()
      on.exit({
        w$hide()
      })
      Sys.sleep(2)
      generate_chart(
        filepaths = filepaths_results,
        plot_type = plot_type(),
        plot_series = ifelse(mean_switch(), "mean", "all"),
        colors = colors,
        colors_series = colors_series,
        return_series = FALSE
      )
    })

    output$pft_chart <- ecs.render(
      chart_reactive()
    )
  })
}

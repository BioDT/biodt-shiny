box::use(
  shiny[NS, moduleServer, tags],
  bslib[card, card_header, card_body],
  echarty[ecs.output, ecs.render],
)

box::use(
  app / logic / grassland / grassland_echart_modular[generate_chart]
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
    class = "mx-md-3 card-shadow mb-2",
    full_screen = TRUE,
    card_header(
      tags$h2(
        class = "card_title",
        i18n$translate("PFTs Chart"))
    ),
      ecs.output(
        ns("pft_chart"),
        width = plot_width,
        height = plot_height
      )
    )
}

#' @export
grassland_dynamics_datachart_server <- function(id) { # nolint
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Parameters for chart ----
    filepaths_results <- list.files("app/data/grassland/simulations/project1/output", full.names = TRUE)
    stack <- "total"
    series_opacity <- 0.01
    colors <- c("#00ab4a", "#ae0000", "#003fc8")
    colors_series <- c("#b4e4b4", "#dfa7a7", "#9c9cdf")

    output$pft_chart <- ecs.render({
      generate_chart(
        filepaths = filepaths_results,
        plot_type = "line",
        plot_series = "mean",
        colors = colors,
        colors_series = colors_series,
        return_series = FALSE
      )
    })
  })
}

box::use(
  shiny[NS, moduleServer, tags],
  bslib[card, card_header, card_body],
  echarty[ecs.output, ecs.render, ec.init],
  readr[read_delim]
)

box::use(
  app/logic/grassland/grassland_process_data_for_chart[grassland_data_plot]
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

# TODO!!!!
# output$pft_chart <- ecs.render({
#   p <- ec.init()

  })

}

box::use(
  shiny[moduleServer, NS, tagList, tags],
  bslib[card, card_header],
  echarty[ecs.output, ecs.render],
)

#' @export
echarty_ui <- function(
    id,
    card_header = "Output plot",
    title = "output_plot",
    plot_width = "100%",
    plot_height = "500px") {
  ns <- NS(id)
  tagList(
    card(
      class = "mx-md-3 card-shadow",
      id = ns("echarty_card"),
      title = title,
      full_screen = TRUE,
      card_header(
        tags$h5(card_header)
      ),
      ecs.output(
        ns("echarty_plot"),
        width = plot_width,
        height = plot_height
      )
    )
  )
}

#' @export
echarty_server <- function(id,
                           echarty_plot) {
  moduleServer(id, function(input, output, session) {
    output$echarty_plot <-
      ecs.render(
        echarty_plot()
      )
  })
}

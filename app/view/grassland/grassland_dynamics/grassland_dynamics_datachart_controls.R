box::use(
  shiny[NS, moduleServer, tags, radioButtons, observeEvent, req],
  bslib[card, card_header, card_body],
)

#' @export
grassland_dynamics_datachart_controls_ui <- function(id, i18n) {
  ns <- NS(id)
  card(
    id = ns("controls"),
    class = "me-md-3 card-shadow mb-2",
    full_screen = FALSE,
    card_header(
      tags$h2(
        class = "card_title",
        i18n$translate("Controls")
      )
    ),
    card_body(
      radioButtons(
        inputId = ns("line_or_bar"),
        label = i18n$translate("Show line(s) or bars in the chart?"),
        choices = list(
          "Lines" = "line",
          "Bars" = "bar"
        )
      )
    )
  )
}

#' @export
grassland_dynamics_datachart_controls_server <- function(id, plot_type) { # nolint
  moduleServer(id, function(input, output, session) {

    observeEvent(input$line_or_bar, ignoreInit = TRUE, {
      req(input$line_or_bar)
      plot_type(input$line_or_bar)
    })
  })
}

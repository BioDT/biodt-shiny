box::use(
  shiny[NS, moduleServer, tags, radioButtons, observeEvent, req, observe],
  bslib[card, card_header, card_body, input_switch],
  shinyjs[disabled, toggleState]
)

#' @export
grassland_dynamics_double_chart_controls_ui <- function(id, i18n) {
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
          "Bars" = "bar",
          "Lines" = "line"
        ),
        selected = "bar"
      ),
      disabled(
        input_switch(
          id = ns("lines_mean_switch"),
          label = "Show mean only",
        )
      )
    )
  )
}

#' @export
grassland_dynamics_double_chart_controls_server <- function(id, plot_type, mean_switch) { # nolint
  moduleServer(id, function(input, output, session) {
    observeEvent(input$line_or_bar, ignoreInit = TRUE, {
      req(input$line_or_bar)
      plot_type(input$line_or_bar)
    })

    observe({
      toggleState(id = "lines_mean_switch", condition = input$line_or_bar == "line")
    })

    observeEvent(input$lines_mean_switch, ignoreInit = TRUE, {
      mean_switch(input$lines_mean_switch)
    })
  })
}

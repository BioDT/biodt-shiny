box::use(
  shiny[NS, moduleServer, tags, radioButtons, observeEvent, req, observe],
  bslib[card, card_header, card_body, input_switch],
)

box::use(
  app / logic / translate_multiple_choices[translate_multiple_choices],
)

plot_types <- c(
  "Bars - mean over all runs" = "bar",
  "Lines - mean over all runs" = "line_mean",
  "Lines - all runs" = "line"
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
        label = i18n$translate("PFT plot type:"),
        choices = plot_types,
        selected = "bar"
      )
    )
  )
}

#' @export
grassland_dynamics_double_chart_controls_server <- function(id, plot_type, i18n) {
  # nolint
  moduleServer(id, function(input, output, session) {
    observe({
      translate_multiple_choices(
        session,
        "radio",
        input_id = "line_or_bar",
        label = "PFT plot type:",
        inline = FALSE,
        i18n,
        choices_type = "namedlist",
        selected_choice = input$line_or_bar,
        plot_types
      )
    })

    observeEvent(input$line_or_bar, ignoreInit = TRUE, {
      req(input$line_or_bar)
      plot_type(input$line_or_bar)
    })
  })
}

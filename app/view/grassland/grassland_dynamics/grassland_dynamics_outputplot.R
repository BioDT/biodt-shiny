box::use(
  shiny[NS, plotOutput, selectInput, actionButton, moduleServer, renderPlot, tags],
  bslib[card, card_header, card_body, layout_column_wrap],
  shinyjs[disabled],
)

#' @export
grassland_dynamics_outputplot_ui <- function(id, i18n) { # nolint
  ns <- NS(id)
  card(
    id = ns("outputplot"),
    class = "mx-md-3 card-shadow mb-2",
    full_screen = TRUE,
    card_header(
      tags$h2(
        i18n$translate("Model Output"),
        class = "card_title"
      )
    ),
    card_body(
      layout_column_wrap(
        width = 1 / 3,
        selectInput(
          ns("output_list"),
          label = i18n$translate("Choose output dataset"),
          choices = NULL
        ),
        selectInput(
          ns("output_files_list"),
          label = i18n$translate("Choose output files"),
          choices = NULL,
          multiple = TRUE
        ),
        disabled(
          actionButton(
            ns("update_output"),
            label = i18n$translate("Show results"),
            class = "mt-auto"
          )
        )
      ),
    ),
    plotOutput(
      ns("output_plot_gl")
    )
  )
}

#' @export
grassland_dynamics_outputplot_server <- function(id) { # nolint
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

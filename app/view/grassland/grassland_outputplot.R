box::use(
  shiny[NS, plotOutput, selectInput, actionButton, moduleServer, renderPlot],
  bslib[card, card_header, card_body, layout_column_wrap],
  shinyjs[disabled],
)

#' @export
grassland_outputplot_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    class = "mx-md-3 card-shadow",
    id = ns("output_plot"),
    full_screen = TRUE,
    card_header(
      ("Model Output")
    ),
    card_body(
      bslib::layout_column_wrap(
        width = 1 / 3,
        shiny::selectInput(
          ns("output_list"),
          label = "Choose output dataset",
          choices = NULL
        ),
        shiny::selectInput(
          ns("output_files_list"),
          label = "Choose output files",
          choices = NULL,
          multiple = TRUE
        ),
        shinyjs::disabled(
          shiny::actionButton(
            ns("update_output"),
            label = "Show results"
          )
        )
      ),
    ),
    shiny::plotOutput(
      ns("output_plot_gl")
    )
  )
}

#' @export
grassland_outputplot_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$output_plot_gl <- shiny::renderPlot(
      print("Grasslands' shiny::renderPlot run")
    )
  })
}

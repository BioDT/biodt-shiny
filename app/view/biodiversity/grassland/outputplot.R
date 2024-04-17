box::use(
  shiny[NS, plotOutput, selectInput, actionButton, moduleServer, renderPlot, tags],
  bslib[card, card_header, card_body, layout_column_wrap],
  shinyjs[disabled],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  card(
    id = ns("outputplot"),
    class = "mx-md-3 card-shadow mb-2",
    
    full_screen = TRUE,
    card_header(
      tags$h5(("Model Output"))
    ),
    card_body(
      layout_column_wrap(
        width = 1 / 3,
        selectInput(
          ns("output_list"),
          label = "Choose output dataset",
          choices = NULL
        ),
        selectInput(
          ns("output_files_list"),
          label = "Choose output files",
          choices = NULL,
          multiple = TRUE
        ),
        disabled(
          actionButton(
            ns("update_output"),
            label = "Show results",
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
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$output_plot_gl <- renderPlot(
      print("Grasslands' shiny::renderPlot run")
    )
  })
}

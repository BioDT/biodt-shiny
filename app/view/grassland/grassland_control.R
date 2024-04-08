box::use(
  shiny[moduleServer, NS, tags, actionButton],
  bslib[card, card_header]
)

#' @export
grassland_control_ui <- function(id) {
  ns <- NS(id)
  card(
    id = ns("control"),
    class = "mt-2 mx-md-3 card-shadow",
    card_header(
      tags$div(
        class = "row d-flex justify-content-between align-items-center my-1",
        tags$div(
          class = "col-md-8 col-sm-12 me-auto",
          tags$h5("Control"),
        ),
        tags$div(
          class = "col-md-4 col-sm-12 d-flex flex-row justify-content-end",
          actionButton(
            ns("run_workflow"),
            label = "Run Workflow",
            width = "100%",
            class = "btn-secondary",
            style = "max-width: 200px"
          ),
        )
      )
    )
  )
}

#' @export
grassland_control_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

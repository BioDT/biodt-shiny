box::use(
  shiny[moduleServer, NS, tags, actionButton],
  bslib[card, card_header]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  card(
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
          # actionButton(
          #   ns("update_map"),
          #   label = "Update map",
          #   width = "100%",
          #   class = "btn-secondary mx-2",
          # ),
        )
      )
    )
  )
}

# TODO
# #' @export
# server <- function(id) {
#   moduleServer(id, function(input, output, session) {

#   })
# }

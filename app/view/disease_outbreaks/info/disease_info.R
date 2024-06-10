box::use(
  shiny[NS, column, tags, fluidRow],
)

#' @export
disease_info_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    id = ns("welcome"),
    class = "align-items-center justify-content-center",
    column()
  )
}

#' @export
disease_info_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  })
}

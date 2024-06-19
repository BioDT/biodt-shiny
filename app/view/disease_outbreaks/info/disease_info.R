box::use(shiny[NS, column, tags, fluidRow], )

#' @export
disease_info_ui <- function(id, i18n) {
  ns <- NS(id)
  fluidRow(
    id = ns("welcome"),
    class = "align-items-center justify-content-center",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center",
        tags$h2(
          class = "greeting display-4 font-weight-bold",
          i18n$translate("Disease Outbreaks")
        ),
        tags$h6(i18n$translate("Subheading - Disease Outbreaks")),
        tags$p(
          class = "pt-3",
          i18n$translate("[EN] Lorem ipsum...")
        )
      ),
    ),
    column(
      width = 6,
      tags$div(
        class = "m-0 p-0 d-none d-lg-block",
        tags$img(
          src = "static/img/wild_boar.jpeg",
          style = "width: 1000px;"
        )
      )
    )
  )
}

#' @export
disease_info_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

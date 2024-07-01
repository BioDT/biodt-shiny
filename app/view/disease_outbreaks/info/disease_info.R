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
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          src = "static/img/Alexis-Lours-Sus-scrofa-Linnaeus.gif",
          style = "width: 50vw; height: 100vh; max-height: 1000px; object-fit: cover;"
        ),
        tags$p(
          "Credits: fabd06, Sus scrofa Linnaeus, 1758 observed in France, https://www.inaturalist.org/observations/197457782")
      )
    )
  )
}

#' @export
disease_info_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

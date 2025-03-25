box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon],
)


#' @export
rtbm_contributors_ui <- function(id, i18n = NULL) {
  ns <- NS(id)
  fluidRow(
    class = "align-items-center justify-content-center m-0 p-0",
    style = "overflow-x: hidden",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      style = "height: 100vh;",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center mt-5",
        tags$h2(
          if (!is.null(i18n)) i18n$translate("CONTRIBUTORS") else "Contributors",
          style = "greeting display-4 font-weight-bold"
        ),
        tags$p("Name and affiliation"),
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img()
      )
    )
  )
}

#' @export
rtbm_contributors_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

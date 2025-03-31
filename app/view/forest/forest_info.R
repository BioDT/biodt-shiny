box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon, actionButton, observeEvent],
  bslib[nav_select],
)

#' @export
forest_info_ui <- function(id, i18n) {
  ns <- NS(id)
  fluidRow(
    class = "fluid-row",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      style = "height: 100vh;",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center mt-5 mb-5",
        tags$h2(
          class = "greeting display-4 font-weight-bold",
          i18n$translate("Forest/Bird Prototype Digital Twin (DT)")
        ),
        tags$p(
          i18n$translate("Source code and scripts of the pDT can be found at "),
          tags$a(icon("github"), "https://github.com/BioDT", href = "https://github.com/BioDT"),
          "."
        ),
        tags$div(
          class = "mt-5",
          actionButton(
            ns("start"),
            label = i18n$translate("Start prototyping"),
            width = "100%",
            class = "btn-secondary",
            style = "max-width: 200px"
          )
        )
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          class = "info-picture",
          src = "static/img/forest/trees-975091_1280.jpg",
          alt = "Forest. Image by Robert Balog from Pixabay",
        )
      )
    )
  )
}


#' @export
forest_info_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    observeEvent(
      input$start,
      {
        nav_select(
          "tab",
          selected = "App",
          session = main_session
        )
      }
    )
  })
}

box::use(
  shiny[moduleServer, NS, strong, em, h2, p, tagList, div, column, tags, fluidRow, icon, actionButton, observeEvent,showNotification],
  bslib[nav_select],
)

#' @export
ias_info_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    class = "fluid-row",
    style ="overflow-x: hidden",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      style = "height: 100vh;",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center mt-5 mb-5",
        h2("Welcome to the dashboard for the Invasive Alien Species prototype Digital twin."),
        p("This dashboard provides insights into invasive alien species in Europe and related statistics."),
        p("Use the navigation tabs to explore detailed visualizations and data summaries."),
        p(strong("v0.0.5:"), em("last updated on"), em("2025-01-28"))
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          class = "info-picture",
          src = "static/img/ias/IAS.png",
          alt = "",
        )
      )
    )
  )
}

#' @export
ias_info_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {

    observeEvent(
      input$start,
      {
        nav_select(
          "tab",
          selected = "ias_app",
          session = main_session
        )
      }
    )
  })
}

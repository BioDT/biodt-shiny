box::use(
  shiny[moduleServer, NS, strong, em, h2, p, tagList, div, column, tags, fluidRow, icon, actionButton, observeEvent,showNotification],
  bslib[nav_select],
)

#' @export
rtbm_info_ui <- function(id) {
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
        h2("Welcome to the Real-time bird monitoring pDT dashboard."),
        p("This dashboard provides insights into bird observations and related statistics. \n Use the navigation tabs to explore detailed visualizations and data summaries."),
        p(strong("v0.1.2:"), em("updated on 2025-02-20"))
)
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          class = "info-picture",
          src = "static/img/rtbm/bird.webp",
          alt = "",
        )
      )
    )
  )
}

#' @export
rtbm_info_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {

    observeEvent(
      input$start,
      {
        nav_select(
          "tab",
          selected = "rtbm_app",
          session = main_session
        )
      }
    )
  })
}

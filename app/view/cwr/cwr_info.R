box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon, actionButton, observeEvent, showNotification],
  bslib[nav_select],
)

#' @export
cwr_info_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    class = "align-items-center justify-content-center m-0 p-0",
    style = "overflow-x: hidden",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      style = "height: 100vh;",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center mt-5 mb-5",
        tags$h2(
          class = "greeting display-4 font-weight-bold",
          "Crop Wild Relatives pDT"
        ),
        tags$p(
          class = "pt-3",
          "The CWR prototype Digital Twin (pDT) in EARLY ACcwrS"
        ),
        tags$h2("This prototype digital twin is in early accwrs and intended for research purposes only; do not use for decision-making or operational purposes."),
        tags$p("Pariatur ut maiores qui. Aut sint quia quis minus natus temporibus modi deleniti. Eum excepturi maiores repudiandae neque. Quidem perspiciatis et laudantium similique illo eaque. Excepturi eius atque aspernatur excepturi dolore modi adipisci maxime")
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          src = "static/img/cwr/2048px-Cairngorms_National_Park_road_(Unsplash).jpg",
          style = "width: 50vw; height: 100vh; max-height: 1000px; object-fit: cover;"
        )
      )
    )
  )
}




#' @export
cwr_info_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    observeEvent(
      input$start,
      {
        nav_select(
          "tab",
          selected = "cwr-map",
          session = main_session
        )
      }
    )
  })
}

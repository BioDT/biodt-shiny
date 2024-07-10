box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon, actionButton, observeEvent],
  bslib[nav_select],
)

#' @export
ces_info_ui <- function(id) {
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
        tags$h2(
          class = "greeting display-4 font-weight-bold",
          "Cultural Ecosystem Services pDT"
        ),
        tags$p(
          class = "pt-3",
          "The CES prototype Digital Twin (pDT)"
        ),
        tags$p("Explore the Digital Twin for Cultural Ecosystems! Our digital twin is designed to enhance your understanding and management of cultural ecosystem services. These services encompass the intangible benefits derived from nature, such as recreation, tourism, intellectual growth, spiritual fulfillment, contemplation, and aesthetic enjoyment.
  Using a recreation potential model, we assess the cultural ecosystem services of the landscape, while species distribution models quantify the biodiversity aspect.")
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          class = "info-picture",
          src = "static/img/ces/2048px-Cairngorms_National_Park_road_(Unsplash).jpg",
          alt = "",
        )
      )
    )
  )
}




#' @export
ces_info_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    observeEvent(
      input$start,
      {
        nav_select(
          "tab",
          selected = "CES",
          session = main_session
        )
      }
    )
  })
}

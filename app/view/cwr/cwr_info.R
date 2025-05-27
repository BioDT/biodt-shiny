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
          "The Crop Wild Relatives Digital Twin (CWR DT) aims to support crop breeding programs by broadening the genetic base of cultivated crops—ultimately enhancing their resilience to climate change–induced environmental stresses."
        ),
        tags$p(
          "Using environmental profiling techniques and species distribution modeling (SDM), the CWR DT identifies, and maps populations of wild relatives adapted to extreme conditions such as high or low temperatures, soil acidity, salinity, drought, and more."
        ),
        tags$p(
          "By highlighting these marginally thriving CWR populations, the digital twin enables breeders to tap into valuable traits for widening the genetic bases of crops and strengthening crops' resilience and adaptability."
        ),
        tags$p(
          "Read more: ",
          tags$a(
            "https://doi.org/10.3897/rio.10.e125192",
            href = "https://doi.org/10.3897/rio.10.e125192",
            target = "_blank"
          )
        ),
        tags$p(
          "Source code and scripts of the pDT can be found at ",
          tags$a(icon("github"), "https://github.com/BioDT", href = "https://github.com/BioDT", target = "_blank"),
          "."
        ),
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          src = "static/img/ces/2048px-Cairngorms_National_Park_road_(Unsplash).jpg",
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

box::use(
  shiny[moduleServer, NS, column, tags, fluidRow, icon, actionButton, observeEvent],
  bslib[nav_select],
  htmltools[tagList, div, strong, em]
)

#' @export
rtbm_info_ui <- function(id, i18n = NULL) {
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
          if (!is.null(i18n)) i18n$translate("Real-Time Bird Monitoring Prototype Digital Twin (DT)") else "Real-Time Bird Monitoring Prototype Digital Twin (DT)"
        ),
        tags$p(
          class = "pt-3",
          "The Real-Time Bird Monitoring (RTBM) prototype Digital Twin (pDT) provides an interactive platform for visualizing and analyzing bird species distributions across Finland. Leveraging real-time and historical observation data, this dashboard enables researchers, conservationists, and enthusiasts to explore bird migration patterns, population trends, and species diversity with intuitive controls and dynamic visualizations."
        ),
        tags$p(
          strong("Data sources:"),
          " Bird observation data are provided by national and international monitoring programs, including citizen science initiatives. For more details, see ",
          tags$a("BioDT RTBM Use Case", href = "https://biodt.eu/use-cases/real-time-bird-monitoring-citizen-science-data", target = "_blank"), ". Data are updated regularly to reflect the latest findings."
        ),
        tags$p(
          strong("Open Science:"),
          " The RTBM pDT is developed as part of the BioDT project, with source code and documentation available on ",
          tags$a("GitHub", href = "https://github.com/BioDT/biodt-shiny", target = "_blank"), "."
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
          src = "static/img/rtbm/bird.webp",
          alt = ""
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

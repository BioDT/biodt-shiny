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
        class = "col-sm-10 offset-sm-1 text-left mt-5 mb-5",
        h2("Overview:"),
        p("The Prototype Biodiversity Digital Twin for Invasive Alien Species (IAS pDT) provides dynamic projections of the potential distribution of terrestrial naturalised alien plant species across Europe."),
        p('Leveraging joint species distribution models (jSDMs) and species occurrence data from GBIF and EASIN, IAS pDT estimates individual-species habitat suitability and the level of plant invasion in selected habitats under current and future climate scenarios.'),
        h2("Key Features:"),
        tags$ul(
          tags$li("Regularly updated outputs – Ensures timely and relevant information about invasion risks."),
          tags$li("High-Risk Area Identification – Detects areas for spatial multispecies prioritisation and proactive intervention."),
          tags$li("Habitat Suitability Estimation – Enables targeted management and eradication."),
          tags$li("Climate Scenario Projecting – Assesses invasion risks under different climate conditions and supports long-term strategic management."),
          tags$li("Prediction Uncertainty Quantification – Enhances decision-making confidence."),
          tags$li("Maps and data download – Enables external use and knowledge transfer.")
        ),
        p(strong("v0.0.9:"), em("last updated on"), em("2025-03-29"))
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          class = "info-picture",
          src = "static/img/ias/IAS2.png",
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

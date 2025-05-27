box::use(
  shiny[
    moduleServer,
    NS,
    strong,
    em,
    h2,
    p,
    tagList,
    div,
    column,
    tags,
    fluidRow,
    icon,
    actionButton,
    observeEvent,
    showNotification
  ],
  bslib[nav_select],
)

#' @export
ias_info_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    class = "fluid-row",
    style = "overflow-x: hidden",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      style = "height: 100vh;",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center mt-5 mb-5",
        h2("Invasive Alien Species Digital Twin (DT)"),
        p(
          "This dashboard provides projections of the distribution of invasive alien plant species (IAS) in Europe under current and future climate scenarios. Species distributions were modelled at the broad-habitat level using joint species distribution models. For each habitat type, the dashboard shows interactive gridded maps for the level of plant invasion (i.e., projected number of IAS) and species-specific habitat suitability. For more information on the IAS pDT, see Khan et al. 2024."
        ),
        p(
          tags$strong("Source code: "),
          tags$a("https://github.com/BioDT/IASDT.R", href = "https://github.com/BioDT/IASDT.R", target = "_blank"),
          " and ",
          tags$a(
            "https://github.com/BioDT/uc-ias-workflows",
            href = "https://github.com/BioDT/uc-ias-workflows",
            target = "_blank"
          )
        ),
        p(
          tags$strong("Licence: "),
          "The data may be used freely for scientific and policy purposes under the ",
          tags$a(
            "Creative Commons Licence CC BY 4.0",
            href = "https://creativecommons.org/licenses/by/4.0/",
            target = "_blank"
          )
        ),
        p(
          tags$strong("Contact: "),
          "For more information, please contact: ",
          tags$a("Ahmed El-Gabbas", href = "mailto:elgabbas@outlook.com"),
          " (data & models); ",
          tags$a("Taimur Khan", href = "mailto:taimur.khan@ufz.de"),
          " (pDT workflow & data server)."
        ),
        p(
          tags$strong("Citations: "),
          "soon"
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

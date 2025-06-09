box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon, actionButton, observeEvent, showNotification],
  bslib[nav_select],
)

#' @export
ces_info_ui <- function(id, i18n) {
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
        tags$h2(
          class = "greeting display-4 font-weight-bold",
          "Cultural Ecosystem Services Prototype Digital Twin"
        ),
        tags$p(
          class = "pt-3 fw-bold",
          i18n$translate(
            "This prototype Digital Twin is in early access and intended for research purposes only. Do not use for decision-making or operational purposes!"
          )
        ),
        tags$p(
          "This digital twin is designed to enhance your understanding and management of cultural ecosystem services. These services encompass the intangible benefits derived from nature, such as recreation, tourism, intellectual growth, spiritual fulfillment, contemplation, and aesthetic enjoyment.
  Using a recreation potential model, we assess the cultural ecosystem services of the landscape, while species distribution models quantify the biodiversity aspect."
        ),
        tags$p(
          "The pDT is described in ",
          tags$a(
            "Rolph S, Andrews C, Carbone D, Lopez Gordillo J, Martinovič T, Oostervink N, Pleiter D, Sara-aho K, Watkins J, Wohner C, Bolton W, Dick J (2024) Prototype Digital Twin: Recreation and biodiversity cultural ecosystem services. Research Ideas and Outcomes 10: e125450. https://doi.org/10.3897/rio.10.e125450",
            href = "https://doi.org/10.3897/rio.10.e125450",
            target = "_blank"
          ),
          "."
        ),
        tags$p(
          "Biodiversity data is accessed from Global Biodiversity Information Facility (GBIF)",
          tags$a("https://www.gbif.org/", href = "https://www.gbif.org/", target = "_blank"),
          "."
        ),
        tags$p(
          "Source code and scripts of the pDT can be found at ",
          tags$a(icon("github"), "https://github.com/BioDT", href = "https://github.com/BioDT", target = "_blank"),
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
          src = "static/img/ces/2048px-Cairngorms_National_Park_road_(Unsplash).jpg",
          alt = "Cairnforms National Park",
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

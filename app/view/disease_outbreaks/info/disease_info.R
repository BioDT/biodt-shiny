box::use(
  shiny[NS, column, tags, fluidRow, icon, actionButton, moduleServer, observeEvent],
  bslib[nav_select],
)

#' @export
disease_info_ui <- function(id, i18n) {
  ns <- NS(id)
  fluidRow(
    id = ns("welcome"),
    class = "fluid-row",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      style = "height: 100vh;",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center mt-5 mb-5",
        tags$h2(
          class = "greeting display-4 font-weight-bold",
          i18n$translate("Disease Outbreak pDT")
        ),
        tags$p(
          class = "pt-3 fw-bold",
          i18n$translate(
            "This prototype Digital Twin is in early access and intended for research purposes only. Do not use for decision-making or operational purposes!"
          )
        ),
        tags$p(
          class = "pt-3",
          i18n$translate("The BioDT wild boar (Sus scrofa)-African swine fever (ASF) prototype digital twin (pDT) implements a stochastic, spatially- and temporally-explicit, and individually-based landscape-level mechanistic model as a digital twin aimed at providing informed support for management decisions in response to the spread of African swine fever in European wild boar populations.")
        ),
        tags$p(
          i18n$translate("The model incorporates wild boar ecology and ASF epidemiology to simulate infection dynamics in the wild boar population. The first run of a wild boar-ASF model simulation is triggered when habitat structure data are supplied to the model. Each time additional information (wild boar locality data, ASF infection detections) is provided, the model reruns, incorporating the new data and returning an updated, dynamic prediction of African swine fever infection including identification of active and recovered cases as well as incidents of wild boar death due to the virus.")
        ),
        tags$p(
          i18n$translate("Read more about the pDT here:"),
          tags$a(
            "https://riojournal.com/article/125521/",
            href = "https://riojournal.com/article/125521/",
            target = "_blank"
          ),
          "."
        ),
        tags$p(
          i18n$translate("Read more about the wild boar-ASF simulation model here:"),
          tags$a(
            "https://doi.org/10.1007/s00477-016-1358-8",
            href = "https://doi.org/10.1007/s00477-016-1358-8",
            target = "_blank"
          ),
          "."
        ),
        tags$p(
          i18n$translate("Video source: Alexis Lours, Sus scrofa Linnaeus, 1758 observed in France,"),
          tags$a(
            "https://www.inaturalist.org/observations/197802817",
            href = "https://www.inaturalist.org/observations/197802817",
            target = "_blank"
          ),
          "."
        ),
        tags$p(
          i18n$translate("Source code and scripts of the pDT can be found at "),
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
      ),
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          class = "info-picture",
          src = "static/img/Alexis-Lours-Sus-scrofa-Linnaeus.gif",
          alt = i18n$translate("Video of wild boar pack"),
        ),
      )
    )
  )
}

##' @export
disease_info_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    observeEvent(
      input$start,
      {
        nav_select(
          "tab",
          selected = "disease_app",
          session = main_session
        )
      }
    )
  })
}

box::use(shiny[NS, column, tags, fluidRow], )

#' @export
disease_info_ui <- function(id, i18n) {
  ns <- NS(id)
  fluidRow(
    id = ns("welcome"),
    class = "align-items-center justify-content-center",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center",
        tags$h2(
          class = "greeting display-4 font-weight-bold",
          i18n$translate("Disease Outbreaks")
        ),
        tags$p(
          class = "pt-3",
          "The BioDT wild boar (Sus scrofa)-African swine fever (ASF) prototype digital twin (pDT)implements a stochastic, spatially- and temporally-explicit, and individually-baselandscape-level mechanistic model as a digital twin aimed at providing informed support for management decisions in response to the spread of African swine fever in European wild boar populations."
        ),
        tags$p("The model incorporates wild boar ecology and ASF epidemiology to simulate infection dynamics in the wild boar population. The first run of a wild boar-ASF model simulation is triggered when habitat structure data are supplied to the model. Each time additional information (wild boar locality data, ASF infection detections) is provided, the model reruns, incorporating the new data and returning an updated, dynamic prediction of African swine fever infection including identification of active and recovered cases as well as incidents of wild boar death due to the virus."),
        tags$p("Read more about the pDT here:", tags$a("https://riojournal.com/article/125521/", href = "https://riojournal.com/article/125521/"), "."),
        tags$p("Read more about the wild boar-ASF simulation model here:", tags$a("https://doi.org/10.1007/s00477-016-1358-8", href = "https://doi.org/10.1007/s00477-016-1358-8"), "."),
        tags$p("Video source: Alexis Lours, Sus scrofa Linnaeus, 1758 observed in France,", tags$a("https://www.inaturalist.org/observations/197802817", href = "https://www.inaturalist.org/observations/197802817"), "."),
       ),
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          src = "static/img/Alexis-Lours-Sus-scrofa-Linnaeus.gif",
          style = "width: 50vw; height: 100vh; max-height: 1000px; object-fit: cover;"
        ),
      )
    )
  )
}

#' @export
disease_info_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

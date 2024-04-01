box::use(
  shiny[NS, fillPage, div, p]
)

#' @export
mod_info_ui <- function(id) {
  ns <- NS(id)
  fillPage(
    div(
      class = "info-text",
      p(
        "The Biodiversity Digital Twin prototype will provide advanced models for simulation and prediction capabilities, through practical use cases addressing critical issues related to global biodiversity dynamics."
      ),
      p(
        "BioDT exploits the LUMI Supercomputer and employs FAIR data combined with digital infrastructure, predictive modelling and AI solutions, facilitating evidence-based solutions for biodiversity protection and restoration."
      ),
      p(
        "The project responds to key EU and international policy initiatives, including the EU Biodiversity Strategy 2030, EU Green Deal, UN Sustainable Development Goals, Destination Earth."
      )
    )
  )
}

#' @export
mod_info_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

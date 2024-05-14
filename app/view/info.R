box::use(
  shiny[NS, fillPage, div, p, moduleServer, tags],
  bslib[card, card_header, card_body],
)

#' @export
mod_info_ui <- function(id) {
  ns <- NS(id)

  tags$div(
    class = "column",
    tags$div(
      class = "row justify-content-around",
      tags$div(
        class = "col-6",
        card(
          id = ns("pdt_beekeeper"),
          class = "mt-2 mx-md-3 card-shadow",
          card_header(
            tags$div(
              class = "row d-flex justify-content-between align-items-center my-1",
              tags$div(
                class = "col-md-8 col-sm-12 me-auto",
                tags$h5("Beehave Prototype Digital Twin"),
              ),
            )
          ),
          card_body(
            tags$div(
              class = "row d-flex justify-content-between",
              tags$p("Go to Honeybee Beekeeper Simulation")
            )
          )
        )
      ),
      tags$div(
        class = "col-6",
      ),
    ),
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

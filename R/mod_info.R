#' info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_info_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::HTML(
      '<head>
        <style>
        body {
          background-image: url("www/forest2.png");
          background-repeat: no-repeat;
          background-size: 100%;
          background-position: bottom;
        }
        </style>
        </head>'
    ),
    shiny::div(
      shiny::p(
        'The Biodiversity Digital Twin prototype will provide advanced models for simulation and prediction capabilities, through practical use cases addressing critical issues related to global biodiversity dynamics.'
      ),
      shiny::p(
        "BioDT exploits the LUMI Supercomputer and employs FAIR data combined with digital infrastructure, predictive modelling and AI solutions, facilitating evidence-based solutions for biodiversity protection and restoration."
      ),
      shiny::p(
        "The project responds to key EU and international policy initiatives, including the EU Biodiversity Strategy 2030, EU Green Deal, UN Sustainable Development Goals, Destination Earth."
      ),
      style = 'color: #bc6c25; background-color: rgba(256,256,256,0.8); border-radius: 25px; padding-left: 10px; padding-right: 10px'
    )
  )
}

#' info Server Functions
#'
#' @noRd
mod_info_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  })
}

## To be copied in the UI
# mod_info_ui("info_1")

## To be copied in the server
# mod_info_server("info_1")

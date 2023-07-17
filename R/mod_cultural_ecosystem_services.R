#' cultural_ecosystem_services UI Function
#'
#' @description A shiny Module for Cultural Ecosystem Services pDT
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_cultural_ecosystem_services_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      "Hello, world!"
    )
 
  )
}
    
#' cultural_ecosystem_services Server Functions
#'
#' @noRd 
mod_cultural_ecosystem_services_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_cultural_ecosystem_services_ui("cultural_ecosystem_services_1")
    
## To be copied in the server
# mod_cultural_ecosystem_services_server("cultural_ecosystem_services_1")

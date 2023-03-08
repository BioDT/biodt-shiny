#' BEEHAVE UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_BEEHAVE_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' BEEHAVE Server Functions
#'
#' @noRd 
mod_BEEHAVE_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_BEEHAVE_ui("BEEHAVE_1")
    
## To be copied in the server
# mod_BEEHAVE_server("BEEHAVE_1")

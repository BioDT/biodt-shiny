#' ias UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ias_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' ias Server Functions
#'
#' @noRd 
mod_ias_server <- function(id,
                           r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ias_ui("ias_1")
    
## To be copied in the server
# mod_ias_server("ias_1")

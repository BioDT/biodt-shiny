#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  
  # Beehave module ----
  mod_beehave_server("beehave")
  
  # cultural ecosystem services module --- 
  mod_cultural_ecosystem_services_server("cultural_ecosystem_services_1")
}

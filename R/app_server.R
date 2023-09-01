#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom r4lexis get_lexis_oauth_token
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  # Define common interactive variables
  r <- reactiveValues(
    lexis_token = NULL,
    lexis_dataset_list = NULL
  )
  
  # Beehave module ----
  mod_beehave_server("beehave",
                     r)
  
  # cultural ecosystem services module --- 
  mod_cultural_ecosystem_services_server("cultural_ecosystem_services_1")
  
  
  # Auth logic ----
  observeEvent(input$login_button,
               {
                 r$lexis_token <- r4lexis::get_lexis_oauth_token()
                 req(r$lexis_token)
                 # Get list of available datasets to the user for biodt_development project
                 r$lexis_dataset_list <- r4lexis::get_dataset_list(
                   r$lexis_token,
                   project = "biodt_development")
               })
}

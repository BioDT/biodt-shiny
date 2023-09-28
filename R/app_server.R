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
    lexis_dataset_list = NULL,
    page_name = NULL,
    user_info = NULL
  )
  
  # Info module ----
  mod_info_server("info",
                  r)
  
  # Computations module ----
  mod_computations_server("computations",
                     r)
  
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
                 
                 r$user_info <- r4lexis::get_lexis_user_info(r$lexis_token)
               })
  
  # Page navigation reactive event that can be passed to modules ----
  observeEvent(input$navbar,
               {
                 r$page_name <- input$navbar
               })
}

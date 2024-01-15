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
    user_info = NULL,
    show_login = TRUE,
    lexis_projects = NULL,
    beehave_user_project = NULL,
    beehave_workflow_id = NULL,
    beehave_map_dataset = NULL,
    beehave_lookup_dataset = NULL
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
  
  # cultural ecosystem services module ---
  mod_grassland_server("grassland",
                       r)
  
  # cultural invasive alien species module ---
  mod_grassland_server("ias",
                       r)
  
  # crop wild relatives module ---
  mod_cwr_server("cwr",
                 r)
  
  # Auth logic ----
  # Redirect part is not working right now, it should be looked into in the future
  # observeEvent(input$login_button,
  #              {
  #                if (golem::app_prod()) {
  #                  r$lexis_token <- r4lexis::get_lexis_oauth_token(
  #                    host_name = "128.214.253.47",
  #                    host_ip = "128.214.253.47"
  #                  )
  #                } else {
  #                   r$lexis_token <- r4lexis::get_lexis_oauth_token()
  #                }
  #                req(r$lexis_token)
  #                # Get list of available datasets to the user for biodt_development project
  #                golem::print_dev("Getting available dataset list.")
  #                r$lexis_dataset_list <- r4lexis::get_dataset_list(
  #                  r$lexis_token,
  #                  project = "biodt_development")
  #
  #                r$user_info <- r4lexis::get_lexis_user_info(r$lexis_token)
  #              })
  
  # This is a backup method with login directly in a shiny app.
  mod_login_server("login_pass",
                   r)
  
  observeEvent(r$lexis_token,
               priority = 100,
               {
                 req(r$lexis_token)
                 
                 # Get user info ----
                 r$user_info <-
                   r4lexis::get_lexis_user_info(r$lexis_token)
                 
                 # Get list of user projects ----
                 r$lexis_projects <-
                   purrr::map_chr(r$user_info$attributes$prj_list,
                                  purrr::pluck,
                                  "PRJ")
                 
                 
                 # Get list of available datasets to the user for biodt_development project ----
                 golem::print_dev("Getting available dataset list.")
                 r$lexis_dataset_list <-
                   r4lexis::get_dataset_list(r$lexis_token)
                 
                 
                 # Set Beehave variables based on user permissions ----
                 if ("biodt_development" %in% r$lexis_projects) {
                   golem::print_dev("Activating BioDT Development project")
                   r$beehave_user_project <- "biodt_development"
                   r$beehave_workflow_id <- "biodt_beehave_karolina"
                   r$beehave_map_dataset <- "Beehave Input Maps"
                   r$beehave_lookup_dataset <-
                     "Beehave Input Lookup"
                 } else if ("biodt_leip_hack" %in% r$lexis_projects) {
                   golem::print_dev("Activating BioDT Leipzig project")
                   r$beehave_user_project <- "biodt_leip_hack"
                   r$beehave_workflow_id <-
                     "biodt_beehave_karolina_leip"
                   r$beehave_map_dataset <-
                     "Beehave Input Maps Leipzig"
                   r$beehave_lookup_dataset <-
                     "Beehave Input Lookup Leipzig"
                 }
                 
               })
  
  # Page navigation reactive event that can be passed to modules ----
  observeEvent(input$navbar,
               {
                 r$page_name <- input$navbar
               })
  
  # Hide login button after user logged in ----
  observeEvent(r$show_login,
               {
                 golem::print_dev("Hiding login navigation.")
                 shinyjs::hide("nav_login")
               })
}

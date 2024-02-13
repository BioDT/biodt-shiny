#' cultural_ecosystem_services UI Function
#'
#' @description A shiny Module for Cultural Ecosystem Services pDT
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import leaflet
mod_cultural_ecosystem_services_ui <- function(id){
  ns <- NS(id)
  
  tagList(bslib::page_fluid(
    class = "p-0",
    bslib::navset_tab(
      
        bslib::nav_panel("Recreation potential",
                bslib::card(
                  title = "rec_pot_map",
                  full_screen = TRUE,
                  card_title("Recreation potential mapping"),
                  card_body(
                    selectInput("persona",
                                "Recreation potential persona",
                                c("
                                  Hard recreationalist",
                                  "Soft recreationalist"
                                  )
                                ),
                    leafletOutput(ns("rec_pot_map"), height = 600),
                    )
                )
      ),
      
      bslib::nav_panel("Biodiversity",
                bslib::card(
                  title = "biodiversity_map",
                  full_screen = TRUE,
                  card_title("Biodiversity mapping"),
                  card_body(leafletOutput(ns("sp_map"), height = 600))
                )
      ),
      
      bslib::nav_panel("Species distribution models",
                bslib::card(
                  title = "sdm_table",
                  full_screen = TRUE,
                  card_title("Species distribution models"),
                  card_body(DT::DTOutput(ns('sp_tbl')))
                )
      )
      
    )
    )
  )
  
}
    
#' cultural_ecosystem_services Server Functions
#' 
#' @importFrom DT renderDT
#' @importFrom shinipsum random_DT random_ggplot
#' @importFrom shinyjs showElement hideElement
#' @importFrom r4lexis download_file_from_dataset extract_dataset get_dataset_file_list
#' @importFrom terra rast plet
#' @importFrom purrr map_chr pluck
#' @importFrom cli hash_md5
#' @importFrom leaflet renderLeaflet
#' @importFrom leaflet.extras addDrawToolbar drawMarkerOptions
#' @importFrom golem print_dev
#' @importFrom cicerone Cicerone
#'
#' @noRd 
mod_cultural_ecosystem_services_server <- function(id,r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Define reactive variables ----
    r_culteco <- reactiveValues(
      name = "value" 
    )
    
    print(r)
    
    observeEvent(r$page_name,
                 {
                   
                   print("lexis_token:")
                   print(r$lexis_token)
                   
                   print("lexis_dataset_list:")
                   print(r$lexis_dataset_list)
                   
                   print("page_name:")
                   print(r$page_name)
                   
                   req(r$lexis_token,
                       r$lexis_dataset_list,
                       r$page_name == "Ecosystem services")
                   
                   print("Required information supplied to authenticate with LEXIS")
                   
                   # Authenticate with Lexis
                   lexis_token <- r$lexis_token
                   lexis_token <- "INSERT TOKEN"
                   
                   # Specify dataset information
                   dataset_name <- "your_dataset_name"
                   project_id <- "your_project_id"
                   
                   # Retrieve dataset list
                   dataset_list <- r$lexis_dataset_list
                   
                   # Find dataset by name
                   dataset <- dataset_list[[dataset_name]]
                   
                   # Retrieve dataset files
                   dataset_files <- get_dataset_file_list(lexis_token, dataset$location$internalID, project_id)
                   
                   # Download dataset files to local directory
                   destination_path <- tempdir()
                   data <- download_file_from_dataset(lexis_token, dataset, destination_path, extract = TRUE)
                   print(data)
                   
                 }
    )
    
    
    
    #recreation potential map
    output$rec_pot_map <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>%
        leaflet::setView( lng = -3.5616, lat = 57.0492, zoom = 9 )
    })  
    
    #species map
    output$sp_map <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>%
        addTiles(urlTemplate="https://api.gbif.org/v2/map/occurrence/density/{z}/{x}/{y}@1x.png?style=orange.marker&bin=hex",
                 attribution ="GBIF") %>%
        leaflet::setView( lng = -3.5616, lat = 57.0492, zoom = 9 )
    }) 
    
    #species list table
    
    # Get data from LEXIS
    
    
    
    sp_list_table <- data.frame(species = c("Bufo bufo","Froggus froggus") ,
                                common_name = c("Toad","Frog"),
                                last_run= c("2023-04-23","2023-04-25"),
                                n_records = c(2042,4320),
                                view_report=rep("View report",2),
                                rerun_model = rep("Rerun model",2))
    
    output$sp_tbl = renderDT(
      sp_list_table, 
      options = list(lengthChange = FALSE),
      colnames = c('Species',"Common name", "Last run", 'Number of records', '',   ''), 
    )
 
  })
}
    
## To be copied in the UI
# mod_cultural_ecosystem_services_ui("cultural_ecosystem_services_1")
    
## To be copied in the server
# mod_cultural_ecosystem_services_server("cultural_ecosystem_services_1")

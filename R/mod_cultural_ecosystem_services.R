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
    theme = biodt_theme,
    class = "p-0",
    bslib::page_navbar(
      #border = FALSE,
      # bslib::page_fluid(
      theme = biodt_theme,
      
      sidebar = bslib::sidebar(
        shiny::h3("Location"),
        shiny::selectInput(ns("site_list"),
                           label = "Choose location",
                           choices = "Cairngorms National Park"),
        # shiny::h3("Workflow"),
        # shinyjs::disabled(shiny::actionButton(ns("run_workflow"),
        #                                       label = "Run Workflow")),
      ),
      
      nav_panel("Recreation potential",
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
      
      nav_panel("Biodiversity",
                bslib::card(
                  title = "biodiversity_map",
                  full_screen = TRUE,
                  card_title("Biodiversity mapping"),
                  card_body(leafletOutput(ns("sp_map"), height = 600))
                )
      ),
      
      nav_panel("Species distribution models",
                bslib::card(
                  title = "sdm_table",
                  full_screen = TRUE,
                  card_title("Species distribution models"),
                  card_body(DT::DTOutput(ns('sp_tbl')))
                )
      )
      
    )
  ))
  
}
    
#' cultural_ecosystem_services Server Functions
#'
#' @noRd 
mod_cultural_ecosystem_services_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
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

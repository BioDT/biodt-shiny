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
  tagList(
    fluidPage(
      titlePanel("Cultural Ecosystem Services Prototype Digital Twin"),
      
      sidebarLayout(
        
        sidebarPanel(
          h3("Controls"),
          selectInput("location", "Location", "Cairngorms National Park"),
          selectInput("taxon", "Species group", c("Birds","Plants")),
          selectInput("layer", "Layer", c("Recreation potential","Biodiversity")),
        ),
        
        mainPanel(
          h1("Map"),
          leafletOutput("map", height = 400)
          
        )
      )
    )
 
  )
}
    
#' cultural_ecosystem_services Server Functions
#'
#' @noRd 
mod_cultural_ecosystem_services_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$map <- renderLeaflet({
      leaflet() %>% 
        addTiles()
    })
 
  })
}
    
## To be copied in the UI
# mod_cultural_ecosystem_services_ui("cultural_ecosystem_services_1")
    
## To be copied in the server
# mod_cultural_ecosystem_services_server("cultural_ecosystem_services_1")

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
mod_cultural_ecosystem_services_ui <- function(id) {
  ns <- NS(id)
  
  tagList(bslib::page_fluid(
    class = "p-0",
    bslib::navset_tab(
      bslib::nav_panel(
        "Recreation potential",
        bslib::card(
          title = "rec_pot_map",
          full_screen = TRUE,
          card_title("Recreation potential mapping"),
          card_body(
            selectInput(
              "persona",
              "Recreation potential persona",
              c("
                                  Hard recreationalist",
                "Soft recreationalist")
            ),
            leafletOutput(ns("rec_pot_map"), height = 600),
          )
        )
      ),
      
      bslib::nav_panel(
        "Biodiversity",
        
        bslib::card(
          title = "biodiversity_choice",
          full_screen = TRUE,
          card_title("What sort of biodiversity do I want to experience?"),
          card_body(
            radioButtons(
              "radio_group_select",
              "I'm interested in",
              c(
                "  All biodiversity" = "all",
                '  Cairngorms "big 5"' = "big5",
                "  Mammals" = "mammals",
                "  Birds" = "birds",
                "  Plants" = "plants",
                "  Amphibians and reptiles" = "herptiles",
                "  Insects" = "insects"
              ),
              inline = T
            )
          )
        ),
        bslib::card(
          title = "biodiversity_map",
          full_screen = TRUE,
          card_title("Where can I find biodiversity?"),
          card_body(leafletOutput(ns("sp_map"), height = 600))
        ),
        bslib::card(
          title = "sdm_table",
          full_screen = TRUE,
          card_title("Species details"),
          card_body(DT::DTOutput(ns('sp_tbl')))
        )
      )
    )
    
  ))
  
}

#' cultural_ecosystem_services Server Functions
#'
#' @importFrom terra rast plet
#' @importFrom purrr map_chr pluck
#' @importFrom cli hash_md5
#' @importFrom leaflet renderLeaflet
#' @importFrom leaflet.extras addDrawToolbar drawMarkerOptions
#' @importFrom golem print_dev
#' @importFrom cicerone Cicerone
#'
#' @noRd
mod_cultural_ecosystem_services_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Define reactive variables ----
    # r_culteco <- reactiveValues(
    #   name = "value"
    # )
    
    # wrap the whole thing in a observe page
    observeEvent(r$page_name,
                 {
                   #RECREATION POTENTIAL MAP
                   #recreation potential map
                   output$rec_pot_map <- renderLeaflet({
                     leaflet() %>%
                       addTiles() %>%
                       leaflet::setView(lng = -3.5616,
                                        lat = 57.0492,
                                        zoom = 9)
                   })
                   
                   #SPECIES MAP
                   #get species data table
                   cairngorms_sp_list <-
                     read.csv("data/uc-ces/biodiversity/cairngorms_sp_list.csv")
                   
                   #species_table
                   output$sp_tbl = renderDT(
                     cairngorms_sp_list,
                     options = list(lengthChange = FALSE),
                     colnames = c('Key', "Count", "Common name", 'Scientific name'),
                   )
                   
                   #get raster files
                   all_sdm_files <-
                     list.files("data/uc-ces/biodiversity/sdms", full.names = T)
                   taxon_ids_from_file_names <-
                     list.files("data/uc-ces/biodiversity/sdms", full.names = F) |>
                     lapply(
                       FUN = function(x) {
                         gsub("prediction_(\\d+)_.*", "\\1", x)
                       }
                     ) |>
                     unlist()
                   sdm_rasts <- terra::rast(all_sdm_files)
                   sdm_rasts <-
                     sdm_rasts[[names(sdm_rasts) == "constrained"]]
                   names(sdm_rasts) <- taxon_ids_from_file_names
                   
                   #calculate biodiversity layer which is shown to the user
                   mean_biodiversity <-
                     sdm_rasts |> terra::app(mean)
                   terra::values(mean_biodiversity)[terra::values(mean_biodiversity) <
                                                      0.1] <- NA
                   
                   
                   
                   #species map
                   output$sp_map <- renderLeaflet({
                     leaflet() %>%
                       addTiles() %>%
                       addRasterImage(
                         mean_biodiversity,
                         group = "Biodiversity Model",
                         opacity = 0.4,
                         colors = "viridis"
                       ) %>%
                       addTiles(
                         urlTemplate = "https://api.gbif.org/v2/map/occurrence/density/{z}/{x}/{y}@1x.png?style=orange.marker&bin=hex",
                         attribution = "GBIF",
                         group = "Biodiversity Data"
                       ) %>%
                       leaflet::setView(lng = -3.5616,
                                        lat = 57.0492,
                                        zoom = 9) %>%
                       addLayersControl(
                         baseGroups = c("Open Street Map"),
                         overlayGroups = c("Biodiversity Model", "Biodiversity Data"),
                         options = layersControlOptions(collapsed = FALSE)
                       ) %>%
                       hideGroup("Biodiversity Data")
                   })
                   

                 })
  })
}

## To be copied in the UI
# mod_cultural_ecosystem_services_ui("cultural_ecosystem_services_1")

## To be copied in the server
# mod_cultural_ecosystem_services_server("cultural_ecosystem_services_1")

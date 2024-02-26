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
    tags$style(
      HTML(
        'table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}'
      )
    ),
    class = "p-0",
    bslib::navset_tab(
      
      # The information tab
      bslib::nav_panel("Information",
                       bslib::card(
                         title = "info_page",
                         full_screen = TRUE,
                         card_title("Information about this prototype Digital Twin"),
                         card_body(
                           p("Explore the Digital Twin for Cultural Ecosystems! Our digital twin is designed to enhance your understanding and management of cultural ecosystem services. These services encompass the intangible benefits derived from nature, such as recreation, tourism, intellectual growth, spiritual fulfillment, contemplation, and aesthetic enjoyment.
Using a recreation potential model, we assess the cultural ecosystem services of the landscape, while species distribution models quantify the biodiversity aspect."),
                           img(src='https://upload.wikimedia.org/wikipedia/commons/d/d7/The_Cairngorms_-_geograph.org.uk_-_1766434.jpg', align = "right")
                         )
                       )
                     ),
      
      
      # the recreation potential tab
      bslib::nav_panel(
        "Recreation potential",
        bslib::card(
          title = "rec_pot_map",
          full_screen = TRUE,
          card_title("Recreation potential mapping"),
          card_body(
            radioButtons(
              ns("persona"),
              "Please a recreation potential persona from the list below:",
              choiceNames = c("Hard recreationalist - visitors who prefer high-adrenaline activities that require a high level of fitness",
                "Soft recreationalist - who prefer “calmer” activities that do not require a high fitness level"),
              choiceValues = c("hard","soft"),
              width = "100%",
              selected = character(0)
            ),
            leafletOutput(ns("rec_pot_map"), height = 600),
          )
        )
      ),
      
      
      # the biodiversity tab
      bslib::nav_panel(
        "Biodiversity",
        
        tags$h2("Cultural Ecosystem Services - Biodiversity"),
        radioButtons(
          ns("radio_group_select"),
          "I'm interested in",
          c(
            "  All biodiversity" = "all",
            "  Mammals" = "mammals",
            "  Birds" = "birds",
            "  Plants" = "plants",
            "  Insects" = "insects"
          ),
          inline = T,
          selected = character(0),
        ),
        
        fluidRow(
          column(6,
                 bslib::card(
                   title = "biodiversity_map",
                   full_screen = TRUE,
                   max_height = "550px",
                   card_title("Map"),
                   card_body(
                     leafletOutput(ns("sp_map"), height = 400, width = "100%"),
                     textOutput((ns("selected_species")))
                   )
                 )
                 
          ),
          column(6,
                 bslib::card(
                   title = "sdm_table",
                   full_screen = TRUE,
                   card_title("Species list"),
                   card_body(DT::DTOutput(ns('sp_tbl'), height = 800),height = "900px"),
                   
                 )
                 
          )
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
    ces_path <- golem::get_golem_options("ces_path")
    
    observe({
      observeEvent(r$page_name, {
        req(r$page_name == "Ecosystem services")
        print("Running ecosystem services page")
        
        tryNotify <- function(x){tryCatch({x},error = function(e){showNotification(e,type="error",closeButton = T,duration = NULL)})}
        
        # RECREATION POTENTIAL MAP
        output$rec_pot_map <- renderLeaflet({
          hard_rec <- tryNotify(rast(paste0(ces_path, "/RP_maps/recreation_potential_HR_4326_agg.tif")))
          soft_rec <- tryNotify(rast(paste0(ces_path, "/RP_maps/recreation_potential_SR_4326_agg.tif")))
          
          hard_pal <- leaflet::colorNumeric(c("#1D4F11", "#CACD68","#6F4660"), terra::values(hard_rec),na.color = "transparent")
          soft_pal <- leaflet::colorNumeric(c("#1D4F11", "#CACD68","#6F4660"), terra::values(soft_rec),na.color = "transparent")
          
          leaflet() %>%
            addTiles() %>%
            leaflet::setView(lng = -3.5616,
                             lat = 57.0492,
                             zoom = 9) %>%
            addLayersControl(
              baseGroups = c("Open Street Map"),
              overlayGroups = c(
                "Hard recreationalist",
                "Soft recreationalist"
              )
            ) %>% 
            addRasterImage(
              hard_rec,
              group = "Hard recreationalist",
              opacity = 0.75,
              colors = hard_pal
            ) %>% 
            addRasterImage(
              soft_rec,
              group = "Soft recreationalist",
              opacity = 0.75,
              colors = soft_pal
            ) %>% 
            hideGroup("Hard recreationalist") %>%
            hideGroup("Soft recreationalist") %>%
            removeLayersControl()
                
        })
        
        observeEvent(input$persona, {
          if(input$persona == "hard"){
            #remove previous layers
            # add hard to map
            leafletProxy(ns("rec_pot_map")) %>%
              hideGroup("Soft recreationalist") %>%
              showGroup("Hard recreationalist")
          } else {
            leafletProxy(ns("rec_pot_map")) %>%
              hideGroup("Hard recreationalist") %>%
              showGroup("Soft recreationalist")
          }
          
        })
        
        # SPECIES MAP
        if(file.exists(paste0(ces_path, "/cairngorms_sp_list.csv"))){
          cairngorms_sp_list <-
            read.csv(paste0(ces_path, "/cairngorms_sp_list.csv"))
        } else {
          showNotification(paste0("File missing: ",paste0(ces_path, "/cairngorms_sp_list.csv")),type= "error",closeButton = T,duration = NULL)
        }
        
        
        all_sdm_files <-
          list.files(paste0(ces_path, "/sdms"), full.names = T)
        
        taxon_ids_from_file_names <-
          list.files(paste0(ces_path, "/sdms"), full.names = F) |>
          lapply(
            FUN = function(x) {
              gsub("prediction_(\\d+)_.*", "\\1", x)
            }
          ) |>
          unlist()
        
        
        files_and_ids <- data.frame(files = all_sdm_files,
                                    ids = taxon_ids_from_file_names)
        
        #which species are selected?
        selected_species <- reactive({
          #ids <- cairngorms_sp_list$speciesKey
          req(input$radio_group_select)
          ids <-
            cairngorms_sp_list[cairngorms_sp_list[, input$radio_group_select] == T,]
        })
        
        
        #create the raster for that group/species
        sdm_rasts <- reactive({
          #
          ids <- selected_species()$speciesKey
          sdm_files <-
            files_and_ids$files[files_and_ids$ids %in% ids]
          sdm_ids <- files_and_ids$ids[files_and_ids$ids %in% ids]
          
          sdm_rasts <- tryNotify(terra::rast(sdm_files))
          sdm_rasts <-
            sdm_rasts[[names(sdm_rasts) == "constrained"]]
          names(sdm_rasts) <- sdm_ids
          sdm_rasts
        })
        
        sdm_rast_total <- reactive({
          req(input$radio_group_select)
          rast_out <- sdm_rasts() |> terra::app(mean)
          terra::values(rast_out)[terra::values(rast_out) < 0.1] <-
            NA
          rast_out
        })
        
        bounding_box <- reactive({
          req(input$radio_group_select)
          print("Map bounds changed")
          bounds <- input$sp_map_bounds
          req(bounds)
          extent <-
            terra::ext(c(
              bounds$west,
              bounds$east,
              bounds$south,
              bounds$north
            ))
          terra::as.polygons(extent, crs = "+proj=longlat")
        })
        
        #ordered list of species you might observe
        species_arranged <- reactive({
          req(input$radio_group_select)
          print("Generating ordered list of species")
          sdm_rasts_used <-
            sdm_rasts() |> terra::crop(bounding_box())
          data.frame(
            speciesKey = as.integer(names(sdm_rasts_used)),
            mean_prob = sdm_rasts_used |> lapply(
              FUN = function(x) {
                terra::values(x) |> mean(na.rm = T)
              }
            ) |> unlist()
          ) |>
            dplyr::arrange(-mean_prob) |>
            dplyr::left_join(cairngorms_sp_list)
        })
        
        
        
        # SPECIES MAP
        output$sp_map <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            leaflet::setView(lng = -3.5616,
                             lat = 57.0492,
                             zoom = 9)  %>%
            addTiles(
              urlTemplate = "https://api.gbif.org/v2/map/occurrence/density/{z}/{x}/{y}@1x.png?style=orange.marker&bin=hex",
              attribution = "GBIF",
              group = "Biodiversity data"
            )  %>%
            addLayersControl(
              baseGroups = c("Open Street Map"),
              overlayGroups = c(
                "Biodiversity hotspots",
                "Biodiversity data",
                "Focal species"
              ),
              options = layersControlOptions(collapsed = FALSE)
            ) %>%
            hideGroup("Biodiversity data") %>%
            hideGroup("Focal species")
        })
        
        #add the biodiversity hotspot layer
        observe({
          req(input$radio_group_select)
          print("Adding layer to leaflet proxy")
          leafletProxy(ns("sp_map")) %>%
            clearGroup("Biodiversity hotspots") %>%
            addRasterImage(
              sdm_rast_total(),
              group = "Biodiversity hotspots",
              opacity = 0.4,
              colors = "viridis"
            )
        })
        
        #render the species info table
        output$sp_tbl <- renderDT(
          species_arranged() |>
            dplyr::mutate(
              likelihood = cut(
                mean_prob,
                breaks = c(0, 0.25, 0.5, 0.75, 1),
                labels = c("Very unlikely", "Unlikely", "Likely", "Very likely")
              ),
              image_url = paste0("<img src='", image_url, "' height='60'></img>")
            ) |>
            dplyr::select(
              "Common name" = common_name,
              "Scientific name" = sci_name,
              #"Number of records" = count,
              #mean_prob
              "Observation probability" = likelihood,
              " " = image_url,
            ),
          escape = FALSE,
          selection = 'single',
          class = 'compact'
        )
        
        # add a single species map
        observeEvent(input$sp_tbl_rows_selected, {
          
          selected_species <- species_arranged()[input$sp_tbl_rows_selected,]
          
          sp_ids_selected <-
            selected_species$speciesKey
          rast_to_add <-
            sdm_rasts()[[as.character(sp_ids_selected)]]
          terra::values(rast_to_add)[terra::values(rast_to_add) < 0.1] <-
            NA
          
          leafletProxy(ns("sp_map")) %>%
            hideGroup("Biodiversity hotspots") %>%
            clearGroup("Focal species") %>%
            showGroup("Focal species") %>%
            addRasterImage(
              rast_to_add,
              group = "Focal species",
              opacity = 0.4,
              colors = "plasma"
            )
          
          output$selected_species <- renderText(
            paste0(
              "Selected species: ",
              selected_species$common_name,
              " (",
              selected_species$sci_name,
              ")"
            )
          )
            
        })
        
      })
    })
  })
}


## To be copied in the UI
# mod_cultural_ecosystem_services_ui("cultural_ecosystem_services_1")

## To be copied in the server
# mod_cultural_ecosystem_services_server("cultural_ecosystem_services_1")

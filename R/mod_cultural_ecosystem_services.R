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
      bslib::nav_panel("Information"),
      bslib::nav_panel(
        "Recreation potential",
        bslib::card(
          title = "rec_pot_map",
          full_screen = TRUE,
          card_title("Recreation potential mapping"),
          card_body(
            selectInput(
              "persona",
              "Please select you a recreation potential persona",
              c("Hard recreationalist",
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
            )
          )
        ),
        
        bslib::layout_column_wrap(
          width = "700px",
          bslib::card(
            title = "biodiversity_map",
            full_screen = TRUE,
            max_height = "500px",
            card_title("Where can I find biodiversity?"),
            card_body(leafletOutput(
              ns("sp_map"), height = 400, width = "100%"
            ))
          ),
          bslib::card(
            title = "sdm_table",
            full_screen = TRUE,
            card_title("Species details"),
            card_body(
              p(
                "Here are the species that you might see if you walk in the area shown in the map above."
              ),
              DT::DTOutput(ns('sp_tbl'),height=800),
              height = 1000
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
      print("Running ecosystem services page")
      
      observeEvent(r$page_name, {
        # RECREATION POTENTIAL MAP
        output$rec_pot_map <- renderLeaflet({
          leaflet() %>%
            addTiles() %>%
            leaflet::setView(lng = -3.5616,
                             lat = 57.0492,
                             zoom = 9)
        })
        
        # SPECIES MAP
        cairngorms_sp_list <-
          read.csv(paste0(ces_path,"/cairngorms_sp_list.csv"))
        
        all_sdm_files <-
          list.files(paste0(ces_path,"/sdms"), full.names = T)
        taxon_ids_from_file_names <-
          list.files(paste0(ces_path,"/sdms"), full.names = F) |>
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
            cairngorms_sp_list[cairngorms_sp_list[, input$radio_group_select] == T, ]
        })
        
        
        #create the raster for that group/species
        sdm_rasts <- reactive({
          #
          ids <- selected_species()$speciesKey
          sdm_files <-
            files_and_ids$files[files_and_ids$ids %in% ids]
          sdm_ids <- files_and_ids$ids[files_and_ids$ids %in% ids]
          
          sdm_rasts <- terra::rast(sdm_files)
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
              overlayGroups = c("Biodiversity hotspots", "Biodiversity data"),
              options = layersControlOptions(collapsed = FALSE)
            ) %>%
            hideGroup("Biodiversity data")
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
        
        #adding a single species map
        # observe({
        #   req(input$radio_group_select)
        #   print("Adding layer to leaflet proxy")
        #   sp_range <- sdm_rasts()[["1422001"]]
        #   terra::values(sp_range)[terra::values(sp_range) < 0.1] <- NA
        #
        #   leafletProxy(ns("sp_map")) %>%
        #     clearGroup("Species models") %>%
        #     hideGroup("Biodiversity hotspots") %>%
        #     addRasterImage(sp_range,
        #       group = "Species models",
        #       opacity = 0.6,
        #       colors = "BuPu"
        #     )
        # })
        
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
              " " = image_url
            ),
          escape=FALSE
        )
        
      })
    })
  })
}


## To be copied in the UI
# mod_cultural_ecosystem_services_ui("cultural_ecosystem_services_1")

## To be copied in the server
# mod_cultural_ecosystem_services_server("cultural_ecosystem_services_1")

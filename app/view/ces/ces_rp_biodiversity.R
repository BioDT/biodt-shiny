box::use(
  shiny[moduleServer, NS, tagList, column, fluidRow, actionButton, observe, observeEvent, radioButtons, p, textOutput, renderText, reactive, HTML, selectInput, req, uiOutput, renderUI, htmlOutput, selectizeInput],
  bslib[card, nav_select, card_title, card_body],
  leaflet[leaflet, leafletOutput, renderLeaflet, leafletProxy, colorBin, layersControlOptions, removeLayersControl, addControl, addLayersControl, clearControls, setView, addTiles, addRasterImage, hideGroup, showGroup, clearGroup, addProviderTiles, providerTileOptions, providers, tileOptions, addLegend, setMaxBounds, labelFormat],
  terra[rast, values, crop, app, ifel, ext, as.polygons, sprc, merge, mean],
  waiter[Waiter],
  DT[renderDT, DTOutput],
  dplyr[mutate, select, arrange, left_join, desc, filter, pull],
  purrr[map_chr],
  cli[hash_md5],
  utils[read.csv],
  stats[setNames],
  shinyjs[useShinyjs, runjs],
  shinyWidgets[virtualSelectInput, pickerInput]
)


# UI function
ces_rp_biodiversity_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(12,  # Enlarge the map to full width
             card(
               title = "combined_map",
               full_screen = TRUE,
               card_title("Recreation & Biodiversity Mapping"),
               card_body(
                 leafletOutput(ns("combined_map_plot"), height = 800, width = "100%")
               )
             )
      )
    )
  )
}

# Server function
ces_rp_biodiversity_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ces_path <- "app/data/ces"
    
    # Waiter for loading screens
    w <- Waiter$new(
      color = "rgba(256,256,256,0.9)"
    )
    
    # Load species list and SDM files
    cairngorms_sp_list <- read.csv(paste0(ces_path, "/cairngorms_sp_list.csv"))
    all_sdm_files <- list.files(paste0(ces_path, "/sdms"), full.names = TRUE)
    taxon_ids_from_file_names <- list.files(paste0(ces_path, "/sdms"), full.names = FALSE) |>
      map_chr(~ gsub("prediction_(\\d+)_.*", "\\1", .x))
    files_and_ids <- data.frame(files = all_sdm_files, ids = taxon_ids_from_file_names)

    # Reactive expression to render the species selector as HTML using pickerInput
    species_selector_html <- reactive({

      species_include = filter(cairngorms_sp_list, speciesKey %in% files_and_ids$ids)

      as.character(
        pickerInput(
          ns("species_selector"),
          "Select species:",
          choices = paste0(species_include$common_name, " (", species_include$sci_name, ")"),
          selected = NULL,
          multiple = TRUE)
      )
    })

    # Reactive expression to get the raster images based on user selections
    rec_pot_map <- reactive({
      w$show()
      hard_rec <- rast(paste0(ces_path, "/RP_maps/recreation_potential_HR_4326_agg.tif"))
      soft_rec <- rast(paste0(ces_path, "/RP_maps/recreation_potential_SR_4326_agg.tif"))
      
      pal <- colorBin("YlGnBu", values(hard_rec), bins = c(0, 0.25, 0.3, 0.33, 0.36, 0.39, 0.45, 1), na.color = "transparent", reverse = FALSE)
      
      biodiversity_pal <- colorBin("YlGnBu", c(0, 1), bins = c(0, 0.25, 0.5, 0.75, 1), na.color = "transparent", reverse = FALSE)
      
      plot <- leaflet() |>
        addTiles(group = "Open Street Map") |>
        addProviderTiles(providers$Esri.WorldImagery, providerTileOptions(zIndex = -1000), group = "ESRI World Imagery") |>
        addProviderTiles(providers$OpenTopoMap, providerTileOptions(zIndex = -1000), group = "Open Topo Map") |>
        setView(lng = -3.5616, lat = 57.0492, zoom = 9) |>
        setMaxBounds(lng1 = -3.860, lat1 = 56.870, lng2 = -3.000, lat2 = 57.290) |>
        addRasterImage(hard_rec, group = "Hard recreationalist", opacity = 0.75, colors = pal, options = tileOptions(zIndex = 1000)) |>
        addRasterImage(soft_rec, group = "Soft recreationalist", opacity = 0.75, colors = pal, options = tileOptions(zIndex = 1000)) |>
        addLegend(pal = biodiversity_pal, values = c(0, 1), title = "Biodiversity", position = "bottomright", 
                  labFormat = labelFormat(prefix = "", suffix = "", between = " - ")) |>
        addLegend(pal = pal, values = values(hard_rec), title = "Recreation", position = "bottomright") |>
        hideGroup("Hard recreationalist") |>
        hideGroup("Soft recreationalist") |>
        addLayersControl(
          baseGroups = c("Open Street Map", "ESRI World Imagery", "Open Topo Map"),
          overlayGroups = c("Hard recreationalist", "Soft recreationalist", "Biodiversity hotspots", "Focal species"),
          options = layersControlOptions(collapsed = FALSE)) |>
        addControl(
          html = HTML(species_selector_html()),
          position = "topleft"
        )
      
      w$hide()
      plot
    })
    
    output$combined_map_plot <- renderLeaflet({
      rec_pot_map()
    })
    
    # Use shinyjs to link the HTML select input with Shiny's reactivity
    observeEvent(input$species_selector, ignoreNULL = FALSE, {

      if (is.null(input$species_selector) || length(input$species_selector) == 0) {
        # Clear the map if no species are selected
        leafletProxy(ns("combined_map_plot")) |>
          clearGroup("Focal species")
      } else {
        
        # Extract scientific names from the selection
        selected_species <- sub(".*\\(([^)]+)\\)", "\\1", input$species_selector)

        # Get the species keys that match the selected scientific names
        selected_species_ids <- filter(cairngorms_sp_list, sci_name %in% selected_species) |> pull(speciesKey)
        
        # Remove previously added species layers to avoid duplication
        leafletProxy(ns("combined_map_plot")) |>
          clearGroup("Focal species")

        # Initialize an empty list to store rasters
        rasters_to_merge <- list()

        for (id in selected_species_ids) {
          file_path <- files_and_ids$files[files_and_ids$ids == id]
          
          if (file.exists(file_path)) {
            rast_to_add <- rast(file_path)[[1]]
            rast_to_add <- ifel(rast_to_add < 0.1, NA, rast_to_add)
            
            # Append to the list of rasters to merge
            rasters_to_merge <- suppressWarnings(append(rasters_to_merge, rast_to_add))
          } else {
            warning(paste("File not found for species ID:", id))
          }
        }

        # Merge all selected rasters into a single raster using 'sprc' and 'merge'
        if (length(rasters_to_merge) > 0) {

          # Merge the rasters
          merged_raster <- terra::mean(rasters_to_merge)
          
          # Add the merged raster to the map
          leafletProxy(ns("combined_map_plot")) |>
            addRasterImage(
              merged_raster,
              group = "Focal species",
              layerId = "merged_species_raster",
              colors = "YlGnBu",
              options = tileOptions(zIndex = 1000),
              opacity = 0.6
            )
        }

        # Show the "Focal species" group in the layers control
        leafletProxy(ns("combined_map_plot")) |>
          showGroup("Focal species")
      }
    })

    # Ensure mutual exclusivity of the Hard and Soft recreationalist layers
    observeEvent(input$combined_map_plot_groups, {
      if ("Hard recreationalist" %in% input$combined_map_plot_groups) {
        leafletProxy(ns("combined_map_plot")) |>
          hideGroup("Soft recreationalist")
      }
      if ("Soft recreationalist" %in% input$combined_map_plot_groups) {
        leafletProxy(ns("combined_map_plot")) |>
          hideGroup("Hard recreationalist")
      }
    })
  })
}
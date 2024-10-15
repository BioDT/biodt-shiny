box::use(
  shiny[moduleServer, NS, tagList, column, fluidRow, actionButton, observe, observeEvent, radioButtons, p, textOutput, renderText, reactive, HTML, selectInput, req, renderUI, htmlOutput, selectizeInput, tags],
  bslib[card, nav_select, card_title, card_body],
  leaflet[leaflet, leafletOutput, renderLeaflet, leafletProxy, colorBin, layersControlOptions, removeLayersControl, addControl, addLayersControl, clearControls, setView, addTiles, addRasterImage, hideGroup, showGroup, clearGroup, addProviderTiles, providerTileOptions, providers, tileOptions, addLegend, setMaxBounds, labelFormat],
  leaflet.extras[addGroupedLayersControl, groupedLayersControlOptions, addControlGPS, gpsOptions],
  terra[rast, values, crop, app, ifel, ext, as.polygons, sprc, merge, mean],
  waiter[Waiter],
  DT[renderDT, DTOutput],
  dplyr[mutate, select, arrange, left_join, desc, filter, pull],
  purrr[map_chr],
  cli[hash_md5],
  utils[read.csv],
  stats[setNames],
  shinyjs[useShinyjs, runjs],
  shinyWidgets[virtualSelectInput, pickerInput, sliderTextInput, updatePickerInput]
)


# UI function
ces_rp_biodiversity_ui <- function(id) {
  ns <- NS(id)
   tagList( 
    fluidRow(
      column(
        12, # Enlarge the map to full width
        card(
          id = "biodiversity-page",
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
    
    w$show()
    
    # Define colours
    recreation_alpha <- 0.5
    biodiversity_alpha <- 0.6
    pal <- colorBin("viridis", c(0, 1), bins = c(0, 0.25, 0.3, 0.33, 0.36, 0.39, 0.45, 1), na.color = "transparent", reverse = FALSE, alpha = biodiversity_alpha)
    biodiversity_pal <- colorBin("magma", c(0, 1), bins = c(0, 0.25, 0.5, 0.75, 1), na.color = "transparent", reverse = FALSE, alpha = recreation_alpha)
    
    # Load species list and SDM files
    cairngorms_sp_list <- read.csv(paste0(ces_path, "/cairngorms_sp_list.csv"))
    all_sdm_files <- list.files(paste0(ces_path, "/sdms"), full.names = TRUE)
    taxon_ids_from_file_names <- list.files(paste0(ces_path, "/sdms"), full.names = FALSE) |>
      map_chr(~ gsub("prediction_(\\d+)_.*", "\\1", .x))
    files_and_ids <- data.frame(files = all_sdm_files, ids = taxon_ids_from_file_names)
    # Load recreation rasters
    hard_rec <- terra::rast(paste0(ces_path, "/RP_maps/recreation_potential_HR_4326_agg.tif"))
    soft_rec <- terra::rast(paste0(ces_path, "/RP_maps/recreation_potential_SR_4326_agg.tif"))
    
    group_species_selector_html <-  tagList(
      pickerInput(
        ns("species_group_selector"),
        "Select species group:",
        choices = c(
          "All biodiversity" = "all",
          "Mammals" = "mammals",
          "Birds" = "birds",
          "Plants" = "plants",
          "Insects" = "insects"),
        selected = "all",
        multiple = FALSE,
        width = "400px"
      ),
    
     pickerInput(
        ns("species_selector"),
        "Select species:",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        width = "400px",
        options = list(
          `live-search` = TRUE,
          `size` = 5,                
          `dropdownAlignRight` = FALSE
        )
      ),
     
    )
    
    # Generate the slider input using tagList
    recreation_occurence_slider_html <- tagList(
      
      sliderTextInput(
        inputId = ns("recreation_potential_slider"),
        label = "Filter Recreation Potential:",
        choices = seq(0, 1, by = 0.1),
        selected = c(0, 1),
        grid = FALSE,
        width = "300px"
      ),
      
      sliderTextInput(
        inputId = ns("species_occurrence_slider"),
        label = "Filter Species Occurrence:",
        choices = seq(0, 1, by = 0.1),
        selected = c(0, 1),
        grid = FALSE,
        width = "300px",
        )
      
    )
    
    # Create the initial leaflet map
    rec_pot_map <- leaflet() |>
      addTiles(group = "Open Street Map") |>
      addProviderTiles(providers$Esri.WorldImagery, providerTileOptions(zIndex = -1000), group = "ESRI World Imagery") |>
      addProviderTiles(providers$OpenTopoMap, providerTileOptions(zIndex = -1000), group = "Open Topo Map") |>
      setView(lng = -3.5616, lat = 57.0492, zoom = 9) |>
      addControlGPS(
        options = gpsOptions(
          position = "topleft",
          activate = TRUE, 
          autoCenter = TRUE,
          setView = TRUE)) |>
      addRasterImage(hard_rec, group = "Hard", colors = pal, options = tileOptions(zIndex = 1000), opacity = recreation_alpha) |>
      addRasterImage(soft_rec, group = "Soft", colors = pal, options = tileOptions(zIndex = 1000), opacity = recreation_alpha) |>
      addLegend(
        pal = biodiversity_pal, values = c(0, 1), title = "Biodiversity", position = "bottomright",
        labFormat = labelFormat(prefix = "", suffix = "", between = " - ")
      ) |>
      addLegend(pal = pal, values = terra::values(hard_rec), title = "Recreation", position = "bottomright") |>
      addControl(
        html = as.character(group_species_selector_html),
        position = "topleft"
      ) |>
      addControl(
        html = as.character(recreation_occurence_slider_html),
        position = "bottomright"
      ) |>
      addGroupedLayersControl(
        position = "bottomright",
        baseGroups = c("Open Street Map", "ESRI World Imagery", "Open Topo Map"),
        overlayGroups = list(
          "Recreationalist" = c("Nothing", "Hard", "Soft"),
          "Biodiversity" = c("Biodiversity hotspots", "Focal species")
        ),
        options = groupedLayersControlOptions(
          collapsed = FALSE,
          exclusiveGroups = "Recreationalist",
          groupsCollapsable = FALSE
        )
      )
    
    w$hide()
    
    # Render the map in leaflet
    output$combined_map_plot <- renderLeaflet({
      
      rec_pot_map
    })
    
    # Update species selector when group is selected
    observeEvent(input$species_group_selector, {
      
      group_selected <- input$species_group_selector
      
      species_include <- cairngorms_sp_list |>
        mutate(in_group = (cairngorms_sp_list |> pull(group_selected))) |>
        filter(speciesKey %in% files_and_ids$ids, in_group == TRUE)
      
      species_choices <- paste0(species_include$common_name, " (", species_include$sci_name, ")")
      
      updatePickerInput(
        session,
        "species_selector",
        selected = NULL,
        choices = species_choices
      )
    })
    
    # Helper function to update species layer
    updateSpeciesLayer <- function() {
      if (is.null(input$species_selector) || length(input$species_selector) == 0) {
        leafletProxy(ns("combined_map_plot")) |> clearGroup("Focal species")
      } else {
        selected_species <- sub(".*\\(([^)]+)\\)", "\\1", input$species_selector)
        selected_species_ids <- filter(cairngorms_sp_list, sci_name %in% selected_species) |> pull(speciesKey)
        
        leafletProxy(ns("combined_map_plot")) |> clearGroup("Focal species")
        
        rasters_to_merge <- list()
        
        for (id in selected_species_ids) {
          file_path <- files_and_ids$files[files_and_ids$ids == id]
          
          if (file.exists(file_path)) {
            rast_to_add <- terra::rast(file_path)[[1]]
            rast_to_add_vals <- terra::values(rast_to_add)
            rast_to_add_filtered <- ifelse(
              rast_to_add_vals >= input$species_occurrence_slider[1] &
                rast_to_add_vals <= input$species_occurrence_slider[2],
              rast_to_add_vals, NA)
            terra::values(rast_to_add) <- rast_to_add_filtered
            
            rasters_to_merge <- c(rasters_to_merge, list(rast_to_add))
          } else {
            warning(paste("File not found for species ID:", id))
          }
        }
        
        if (length(rasters_to_merge) > 0) {
          if (length(rasters_to_merge) == 1) {
            merged_raster <- rasters_to_merge[[1]]
          } else {
            rasters_stack <- terra::rast(rasters_to_merge)
            merged_raster <- terra::app(rasters_stack, fun = mean, na.rm = TRUE)
          }
          
          # Set zero values to NA to prevent black squares
          merged_raster[merged_raster == 0] <- NA
          
          leafletProxy(ns("combined_map_plot")) |>
            addRasterImage(
              merged_raster,
              group = "Focal species",
              layerId = "merged_species_raster",
              colors = biodiversity_pal,
              options = tileOptions(zIndex = 1000),
              opacity = biodiversity_alpha
            )
        }
        
        leafletProxy(ns("combined_map_plot")) |> showGroup("Focal species")
      }
    }
    
    # Observe event to update the map when the species occurrence slider changes
    observeEvent(input$species_occurrence_slider, ignoreNULL = FALSE, {
      
      w$show()
      
      updateSpeciesLayer()
      
      w$hide()
    })
    
    # Observe event to update the map when the species selector changes
    observeEvent(input$species_selector, ignoreNULL = FALSE, {
      
      w$show()
      
      updateSpeciesLayer()
      
      w$hide()
    })
    
    # Observe event to update the map when the recreation potential slider changes
    observeEvent(input$recreation_potential_slider, {
      
      w$show()
      
      # Create copies of the original rasters
      hard_rec_filtered_raster <- hard_rec
      soft_rec_filtered_raster <- soft_rec
      
      # Apply the filter based on the recreation potential slider
      hard_rec_vals <- terra::values(hard_rec)
      soft_rec_vals <- terra::values(soft_rec)
      
      hard_rec_filtered <- ifelse(
        hard_rec_vals >= input$recreation_potential_slider[1] &
          hard_rec_vals <= input$recreation_potential_slider[2],
        hard_rec_vals, NA)
      soft_rec_filtered <- ifelse(
        soft_rec_vals >= input$recreation_potential_slider[1] &
          soft_rec_vals <= input$recreation_potential_slider[2],
        soft_rec_vals, NA)
      
      # Update the raster values in the copies
      terra::values(hard_rec_filtered_raster) <- hard_rec_filtered
      terra::values(soft_rec_filtered_raster) <- soft_rec_filtered
      
      # Update the map with the filtered rasters
      leafletProxy(ns("combined_map_plot")) |>
        clearGroup(c("Hard", "Soft")) |>
        addRasterImage(hard_rec_filtered_raster, group = "Hard", colors = pal, options = tileOptions(zIndex = 1000), opacity = recreation_alpha) |>
        addRasterImage(soft_rec_filtered_raster, group = "Soft", colors = pal, options = tileOptions(zIndex = 1000), opacity = recreation_alpha)
    
      w$hide()
      
      })
    
  })
}

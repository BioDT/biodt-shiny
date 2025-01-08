box::use(
  shiny[tags, moduleServer, NS, tagList, column, fluidRow, actionButton, observe, observeEvent, radioButtons, p, textOutput, renderText, reactive, HTML, selectInput, req, renderUI, htmlOutput, selectizeInput, sliderInput, uiOutput],
  bslib[card, nav_select, card_title, card_body],
  leaflet[leaflet, leafletOptions, leafletOutput, renderLeaflet, leafletProxy, colorBin, layersControlOptions, removeLayersControl, addControl, addLayersControl, clearControls, setView, addTiles, addRasterImage, hideGroup, showGroup, clearGroup, addProviderTiles, providerTileOptions, providers, tileOptions, addLegend, setMaxBounds, labelFormat],
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
    tags$head(

      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"
      ),

      tags$style(HTML("

      .button-container {
       display: flex;
       flex-direction: column;
          gap: 8px;
          position: absolute;
          right: 16px;
          z-index: 1001;
      }

      .button-container.moved {
       right: 300px; /* Adjust this value based on sidebar width */
      }

      .toggle-button {
          background: #414F2F;
          color: #ffffff;
          border: none;
          padding: 8px 15px;
          cursor: pointer;
          box-shadow: 0 2px 5px rgba(0, 0, 0, 0.2);
          transition: background-color 0.3s ease, box-shadow 0.3s ease;
          border-radius: 5px 0 0 5px;
      }

      .toggle-button i {
        font-size: 1.5rem;
      }

         .toggle-button:hover {
          background-color: #556B2F;
      }

      .toggle-button[title] {
          position: relative;
      }


        .toggle-button[title]::after {
            content: attr(title);
            position: absolute;
            right: 100%; /* Position it to the left of the button */
            top: 50%;
            transform: translateY(-50%) translateX(-10px); /* Adjust tooltip to the left */
            background: rgba(0, 0, 0, 0.9);
            color: #fff;
            padding: 6px 10px;
            border-radius: 4px;
            white-space: nowrap;
            opacity: 0;
            pointer-events: none;
            transition: opacity 0.2s ease, transform 0.2s ease;
            z-index: 10;
            min-width: 120px; /* Ensure minimum width */
            word-wrap: break-word; /* Avoid text overflow */
            font-size: 1rem;
        }

        .toggle-button[title]:hover::after {
            opacity: 1;
        }

        .sidebar {
          width: 300px;
          height: 100%;
          background: #fff;
          position: absolute;
          right: -300px; /* Initially hidden off-screen */
          z-index: 1000;
          padding: 40px 15px;
          border-left: 1px solid #ccc;
          overflow-y: auto;
          display: none;
         }

        .sidebar.active {
          right: 0; /* Show sidebar */
          display: block;
        }

        .close-button {
        background: transparent;
        color: #f44336;
        border: none;
        padding: 0;
        cursor: pointer;
        font-size: 1.5rem;
        position: absolute;
        right: 20px;
        top: 0;
      }

    .close-button:hover {
      color: #d32f2f;  /* Darker red icon on hover */
    }

    .leaflet-touch .leaflet-bar a {
    background-color: #414f2f;
    color: white;
    }

.leaflet-control-gps .gps-button {
background-potion: top;
}

      "))
    ),
    fluidRow(
      column(
        12, # Enlarge the map to full width
        card(

          id = "biodiversity-page",
          title = "combined_map",
          full_screen = TRUE,

          card_title("Recreation & Biodiversity Mapping"),

          card_body(
            leafletOutput(ns("combined_map_plot"), height = 800, width = "100%"),

            tags$div(
              class = "button-container",
              actionButton(ns("toggleSliders"), HTML('<i class="fa-solid fa-person-hiking"></i>'), class = "toggle-button", title = "Recreation potential"),
              actionButton(ns("toggleSpecies"), HTML('<i class="fa-solid fa-paw"></i>'), class = "toggle-button", title = "Biodiversity")
            ),
            # Single Sidebar
            tags$div(
              class = "sidebar",
              id = "sidebar",
                tags$div(
                  id = "slidersSidebar",
                  class = "d-none",
                  tags$h3("Sliders Sidebar"),
                  tags$p("Use the sliders below to filter the data:"),
                  sliderTextInput(
                    inputId = ns("recreation_potential_slider"),
                    label = "Filter Recreation Potential:",
                    choices = seq(0, 1, by = 0.1),
                    selected = c(0, 1),
                    grid = FALSE,
                  ),
                  sliderTextInput(
                    inputId = ns("species_occurrence_slider"),
                    label = "Filter Species Occurrence:",
                    choices = seq(0, 1, by = 0.1),
                    selected = c(0, 1),
                    grid = FALSE,
                  )
                ),
                # species content
                tags$div(
                  id = "speciesSidebar",
                  class = "d-none",
                  tags$h3("Species Sidebar"),
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
                    multiple = FALSE
                  ),
                 pickerInput(
                    ns("species_selector"),
                    "Select species:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(
                      `live-search` = TRUE,
                      `size` = 5,
                      `dropdownAlignRight` = FALSE
                    )
                  )
                )
              )
            )
            ),
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

    # Logic for handling the Sliders button click
    observeEvent(input$toggleSliders, {
      runjs('App.toggleSidebar()')  # Call JS to toggle the sidebar for sliders content
      runjs('App.activeRecreation()')
      #runjs('App.deactSpecies')
    })

    # Logic for handling the Species button click
    observeEvent(input$toggleSpecies, {
      runjs('App.toggleSidebar()')  # Call JS to toggle the sidebar for species content
      runjs('App.activeSpecies()')
      #runjs('deactRecreation()')
    })

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
    hard_rec <- terra::rast(paste0(ces_path, "/RP_maps/rec_hard_new.tif"))
    soft_rec <- terra::rast(paste0(ces_path, "/RP_maps/rec_soft_new.tif"))

    group_species_selector_html <-  tagList(
    #   pickerInput(
    #     ns("species_group_selector"),
    #     "Select species group:",
    #     choices = c(
    #       "All biodiversity" = "all",
    #       "Mammals" = "mammals",
    #       "Birds" = "birds",
    #       "Plants" = "plants",
    #       "Insects" = "insects"),
    #     selected = "all",
    #     multiple = FALSE,
    #     width = "400px"
    #   ),


    #  pickerInput(
    #     ns("species_selector"),
    #     "Select species:",
    #     choices = NULL,
    #     selected = NULL,
    #     multiple = TRUE,
    #     width = "400px",
    #     options = list(
    #       `live-search` = TRUE,
    #       `size` = 5,
    #       `dropdownAlignRight` = FALSE
    #     )
    #   ),
  )

    # Create the initial leaflet map
    rec_pot_map <- leaflet(options = leafletOptions(
      scrollWheelZoom = TRUE,
      dragging = TRUE,
      touchZoom = TRUE,
      doubleClickZoom = TRUE,
      closePopupOnClick = FALSE,
      bounceAtZoomLimits = FALSE
    )) |>
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
      addRasterImage(hard_rec, group = "Hard", project = FALSE, colors = pal, options = tileOptions(zIndex = 1000), opacity = recreation_alpha) |>
        hideGroup("Hard") |>
      addRasterImage(soft_rec, group = "Soft", project = FALSE, colors = pal, options = tileOptions(zIndex = 1000), opacity = recreation_alpha) |>
      addLegend(
        pal = biodiversity_pal, values = c(0, 1), title = "Biodiversity", position = "bottomright",
        labFormat = labelFormat(prefix = "", suffix = "", between = " - ")
      ) |>
      addLegend(pal = pal, values = terra::values(hard_rec), title = "Recreation", position = "bottomright") |>
      addControl(
        html = group_species_selector_html,
        position = "topleft"
      ) |>
      # addControl(
      #   html = recreation_occurence_slider_html,
      #   position = "bottomright"
      # ) |>
      addTiles(
        urlTemplate = "https://api.gbif.org/v2/map/occurrence/density/{z}/{x}/{y}@1x.png?style=orange.marker&bin=hex",
        attribution = "GBIF",
        group = "Biodiversity data"
      ) |>
      addGroupedLayersControl(
        position = "bottomright",
        baseGroups = c("Open Street Map", "ESRI World Imagery", "Open Topo Map"),
        overlayGroups = list(
          "Recreationalist" = c("Nothing", "Hard", "Soft"),
          "Biodiversity" = c("Biodiversity data", "Focal species")
        ),
        options = groupedLayersControlOptions(
          collapsed = FALSE,
          exclusiveGroups = "Recreationalist",
          groupsCollapsable = FALSE
        )
      ) |>
      hideGroup("Biodiversity data")

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

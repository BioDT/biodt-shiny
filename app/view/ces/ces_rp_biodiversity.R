box::use(
  shiny[moduleServer, NS, tagList, column, fluidRow, verbatimTextOutput, actionButton, observe, observeEvent, radioButtons, checkboxInput, p, textOutput, renderText, reactive, HTML, selectInput, req, renderUI, htmlOutput, selectizeInput, tags, reactiveVal],
  bslib[card, nav_select, card_title, card_body],
  leaflet[addRasterImage, leafletOutput, renderLeaflet, leafletProxy, colorBin, layersControlOptions, removeLayersControl, addControl, addLayersControl, clearControls, showGroup, clearGroup, setMaxBounds, labelFormat, tileOptions],
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
  shinyWidgets[virtualSelectInput, pickerInput, sliderTextInput, updatePickerInput, awesomeCheckbox],
)

box::use(
  app / logic / ces / ces_map[disease_leaflet_map],
  app / logic / ces / ces_map_update[ces_update_map, update_recreation, update_base_layers, update_species_biodiversity, add_species, show_focal_species],
  app / logic / waiter[waiter_text],
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
          padding: 30px 15px;
          border-left: 1px solid #ccc;
          overflow-y: auto;
          display: none;
        }

        .sidebar.active {
          right: 0; /* Show sidebar */
          display: block;
        }

        .close-button {
        background: none;
        color: #f44336;
        border: none;
        padding: 5px 10px 5px 10px;
        cursor: pointer;
        font-size: 1.5rem;
        position: absolute;
        right: 20px;
        top: 0;
      }

    .close-button:hover {
      color: #d32f2f;  /* Darker red icon on hover */
      background: none;
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
              actionButton(ns("toggleSpecies"), HTML('<i class="fa-solid fa-paw"></i>'), class = "toggle-button", title = "Biodiversity"),
              #actionButton(ns("toggleGrayscale"), HTML('<i class="fa-solid fa-droplet-slash"></i>'), class = "toggle-button", title = "Grayscale map")
            ),
            # Single Sidebar
            tags$div(
              class = "sidebar",
              id = "sidebar",
              actionButton(
                ns("closeButton"),
                class = "close-button",
                HTML('<i class="fa-solid fa-x"></i>'),
                title = "Close Sidebar"
              ),
              # sliders content
                tags$div(
                  id = "slidersSidebar",
                  class = "d-none",
                  tags$h4("Recreation Potential Filter"),
                  tags$p("Use the sliders below to filter the data:"),
                  sliderTextInput(
                    inputId = ns("recreation_potential_slider"),
                    label = "Filter Recreation Potential:",
                    choices = seq(0, 1, by = 0.1),
                    selected = c(0, 1),
                    grid = FALSE,
                  ),
                  actionButton(
                    inputId = ns("apply_filter_recre"),
                    label = "Apply filter",
                    class = "btn-primary"
                  ),
                  tags$h4("Species Selection", class = "mt-3"),
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
                  ),
                  tags$h4("Species Occurrence", class = "mt-3"),
                  sliderTextInput(
                    inputId = ns("species_occurrence_slider"),
                    label = "Filter Species Occurrence:",
                    choices = seq(0, 1, by = 0.1),
                    selected = c(0, 1),
                    grid = FALSE,
                  ),
                  actionButton(
                    inputId = ns("apply_filter_species"),
                    label = "Apply filter",
                    class = "btn-primary"
                  ),
                  tags$h4("Focal Species", class = "mt-3"),
                  checkboxInput(
                    inputId = ns("focal_species"),
                    label = "show focal species",
                    value = FALSE
                  ),
                ),
                # species content
                tags$div(
                  id = "speciesSidebar",
                  class = "d-none",
                  tags$h4("Recreation Potential"),
                  radioButtons(
                    inputId = ns("recreation_potential"),
                    label = "Select recreationist type:",
                    selected = "Soft",
                    choices = list(
                      Soft = "Soft",
                      Hard = "Hard",
                      Empty = "Empty"
                    )
                  ),
                  tags$h4("Base Map Layer", class = "mt-3"),
                  radioButtons(
                    inputId = ns("map_base_layers"),
                    label = "Choose base map:",
                    choices = list(
                      "Open Street Map",
                      "ESRI World Imagery",
                      "Open Topo Map"
                    ),
                    selected = "Open Street Map"
                  ),
                  tags$h4("Biodiversity Data", class = "mt-3"),
                  checkboxInput(
                    inputId = ns("biodiversity"),
                    label = "show biodiversity data",
                    value = FALSE
                  ),
                )
              )
            )
            ),
          )
        )
      )
}

# Server function
ces_rp_biodiversity_server <- function(id, ces_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ces_path <- "app/data/ces"

    # Create reactive variables outside of observeEvent
    key_files <- reactiveVal()
    rec_pot_map <- reactiveVal()
    biodiversity_alpha <- reactiveVal(1)
    recreation_alpha <- reactiveVal(0.8)
    biodiversity_pal <- reactiveVal()
    recreation_pal <- reactiveVal()
    layer_selected <- reactiveVal()
    biodiversity_data_selected <- reactiveVal(FALSE)
    focal_species_merged_raster <- reactiveVal()

    # Waiter for loading screens
    msg <- list(
      waiter_text(message = tags$h3("Loading data...",
                                    style = "color: #414f2f;"
      ))
    )
    w <- Waiter$new(
      html = msg[[1]],
      color = "rgba(256,256,256,0.9)"
    )

    # Logic for handling the Sliders button click
    observeEvent(input$toggleSliders, {
      runjs('App.toggleSidebar()')  # Call JS to toggle the sidebar for sliders content
      runjs('App.activeRecreation()')
    })

    # Logic for handling the Species button click
    observeEvent(input$toggleSpecies, {
      runjs('App.toggleSidebar()')  # Call JS to toggle the sidebar for species content
      runjs('App.activeSpecies()')
    })

    # Logic for basic sidebar closing
    observeEvent(input$closeButton, {
      runjs('App.toggleSidebar()')
    })

    # Only trigger when ces_selected() becomes TRUE (after which the value will not change).
    observeEvent(ces_selected(), {
      w$show()

      # Create palettes
      biodiversity_pal(colorBin("PuBuGn", c(0, 1), bins = seq(0, 1, length.out = 5 + 1), na.color = "transparent", reverse = FALSE, alpha = 1))
      recreation_pal(colorBin("YlOrBr", c(0, 1.5), bins = seq(0, 1, length.out = 5 + 1), na.color = "transparent", reverse = FALSE, alpha = 0.8))

      # Load key files
      key_files_list <- list(cairngorms_sp_list = read.csv(paste0(ces_path, "/cairngorms_sp_list.csv")),
                      files_and_ids = data.frame(
                        files = list.files(paste0(ces_path, "/sdms"), full.names = TRUE),
                        ids = list.files(paste0(ces_path, "/sdms"), full.names = FALSE) |>
                          purrr::map_chr(~ gsub("prediction_(\\d+)_.*", "\\1", .x))
                      ),
                      hard_rec = terra::rast(paste0(ces_path, "/RP_maps/rec_hard_new.tif")),
                      soft_rec = terra::rast(paste0(ces_path, "/RP_maps/rec_soft_new.tif"))
      )

      key_files(key_files_list)

      rec_pot_map_plot <- disease_leaflet_map(
        recre_palette = recreation_pal,
        biodiversity_palette = biodiversity_pal,
        rec_opacity = recreation_alpha,
        key_files = key_files
      )

      rec_pot_map(rec_pot_map_plot)

      w$hide()
    }, ignoreInit = TRUE)

    # Render the map in leaflet
    output$combined_map_plot <- renderLeaflet({
      req(rec_pot_map())

      rec_pot_map()
    })

    # Update species selector when group is selected
    observeEvent(input$species_group_selector, {

      group_selected <- input$species_group_selector

      species_include <- key_files()$cairngorms_sp_list |>
        mutate(in_group = (key_files()$cairngorms_sp_list |> pull(group_selected))) |>
        filter(speciesKey %in% key_files()$files_and_ids$ids, in_group == TRUE)

      species_choices <- paste0(species_include$common_name, " (", species_include$sci_name, ")")

      updatePickerInput(
        session,
        "species_selector",
        selected = NULL,
        choices = species_choices
      )
    }, ignoreInit = TRUE)

    # Helper function to update species layer
    updateSpeciesLayer <- function() {
      if (is.null(input$species_selector) || length(input$species_selector) == 0) {
        ces_update_map("clear_species", ns("combined_map_plot"))
      } else {
        selected_species <- sub(".*\\(([^)]+)\\)", "\\1", input$species_selector)
        selected_species_ids <- filter(key_files()$cairngorms_sp_list, sci_name %in% selected_species) |> pull(speciesKey)

        ces_update_map("clear_species", ns("combined_map_plot"))

        rasters_to_merge <- list()

        for (id in selected_species_ids) {
          file_path <- key_files()$files_and_ids$files[key_files()$files_and_ids$ids == id]

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
          focal_species_merged_raster(merged_raster)

          add_species(
            ns("combined_map_plot"),
            merged_raster,
            biodiversity_pal
          )
        }

        ces_update_map("show_species", ns("combined_map_plot"))
      }
    }

    # Observe event to update the map when the species selector changes
    observeEvent(input$species_selector, ignoreNULL = FALSE, {
      w$show()
      updateSpeciesLayer()

      w$hide()
    })

    # Observe event for "Apply filters" button
    observeEvent(input$apply_filter_recre, {
      w$show()
      # Update recreation raster layers
      hard_rec_vals <- terra::values(key_files()$hard_rec)
      soft_rec_vals <- terra::values(key_files()$soft_rec)
      hard_rec_filtered <- ifelse(
        hard_rec_vals >= input$recreation_potential_slider[1] &
          hard_rec_vals <= input$recreation_potential_slider[2],
        hard_rec_vals, NA)
      soft_rec_filtered <- ifelse(
        soft_rec_vals >= input$recreation_potential_slider[1] &
          soft_rec_vals <= input$recreation_potential_slider[2],
        soft_rec_vals, NA)

      # duplicate the raster adn replace with the hard and soft recreation values
      hard_rec_filtered_raster <- key_files()$hard_rec
      soft_rec_filtered_raster <- key_files()$soft_rec
      terra::values(hard_rec_filtered_raster) <- hard_rec_filtered
      terra::values(soft_rec_filtered_raster) <- soft_rec_filtered

      # Update the map with the filtered rasters
      ces_update_map(
        "filter_recreation",
        ns("combined_map_plot"),
        hard_recreationists_raster = hard_rec_filtered_raster,
        soft_recreationists_raster = soft_rec_filtered_raster,
        recreation_palette = recreation_pal
      )

      w$hide()
    })

    observeEvent(input$apply_filter_species, {
      w$show()
      updateSpeciesLayer()

      w$hide()
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(input$focal_species, {
      w$show()
      show_focal_species(
        input$focal_species,
        ns("combined_map_plot"),
        focal_species_merged_raster,
        biodiversity_pal
      )
      
      w$hide()
    }, ignoreInit = TRUE)

    observeEvent(input$recreation_potential, {
      w$show()

      # Update recreation raster layers
      hard_rec_vals <- terra::values(key_files()$hard_rec)
      soft_rec_vals <- terra::values(key_files()$soft_rec)
      hard_rec_filtered <- ifelse(
        hard_rec_vals >= input$recreation_potential_slider[1] &
          hard_rec_vals <= input$recreation_potential_slider[2],
        hard_rec_vals, NA)
      soft_rec_filtered <- ifelse(
        soft_rec_vals >= input$recreation_potential_slider[1] &
          soft_rec_vals <= input$recreation_potential_slider[2],
        soft_rec_vals, NA)

      # duplicate the raster adn replace with the hard and soft recreation values
      hard_rec_filtered_raster <- key_files()$hard_rec
      soft_rec_filtered_raster <- key_files()$soft_rec
      terra::values(hard_rec_filtered_raster) <- hard_rec_filtered
      terra::values(soft_rec_filtered_raster) <- soft_rec_filtered

      update_recreation(
        input$recreation_potential,
        ns("combined_map_plot"),
        soft_recreationists_raster = soft_rec_filtered_raster,
        hard_recreationists_raster = hard_rec_filtered_raster,
        recreation_palette = recreation_pal
      )

      w$hide()
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    observeEvent(input$map_base_layers, {
      w$show()
      layer_selected(input$map_base_layers)
      update_base_layers(layer_selected(), ns("combined_map_plot"))

      w$hide()
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    
    observeEvent(input$biodiversity, {
      w$show()
      biodiversity_data_selected(input$biodiversity)
      update_species_biodiversity(biodiversity_data_selected(), ns("combined_map_plot"))
    
      w$hide()
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
  })
}

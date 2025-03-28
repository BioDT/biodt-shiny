box::use(
  # Shiny fundamentals and UI
  shiny[
    moduleServer, NS, renderUI, observe, observeEvent,
    isolate, reactiveVal, reactiveValues, req, reactive, renderPlot,
    invalidateLater, updateSliderInput, updateTextInput, withProgress, incProgress, removeUI,
    actionButton, column, fluidRow, selectInput, sliderInput, tagList,
    insertUI, uiOutput, h4, h5, h6, hr, p, pre, plotOutput, dateRangeInput,
    textOutput, renderText, updateDateRangeInput, updateSelectInput, eventReactive,
    icon, div, tags, textInput, showNotification, verbatimTextOutput, renderPrint, HTML
  ],

  # Base R functions needed
  stats[setNames, na.omit, quantile, median, sd, var],

  # HTML structure (using shiny.semantic instead of direct htmltools)
  htmltools[
    a, div, em, HTML, img, p, br, h1, h2, h3,
    strong, tagList, span, hr
  ],

  # Rhinoverse ecosystem UI components
  bslib[
    card, card_header, card_body, card_footer, tooltip, value_box, layout_column_wrap,
    page_fluid, nav_panel, nav_spacer, nav_menu, nav_item, nav, navset_bar,
    as_fill_carrier, as_fill_item
  ],

  # Modern Shiny components (consider replacing with Rhinoverse equivalents when available)
  shinyWidgets[pickerInput, progressBar, updateProgressBar],
  shinyjs[useShinyjs, runjs, hide, show, toggle, addClass, removeClass, toggleClass, delay],

  # Interactive map
  leaflet[
    leaflet, addTiles, setView, addProviderTiles, clearShapes, addCircles,
    addCircleMarkers, addControl, clearControls, addLegend, addPolylines,
    layersControlOptions, addLayersControl, leafletOutput, renderLeaflet, leafletProxy,
    addRasterImage, clearImages, colorNumeric, addMarkers, clearGroup, makeIcon,
    addRectangles, removeControl, removeTiles, hideGroup, showGroup, labelFormat
  ],
  leaflet.extras[addHeatmap],

  # Data manipulation with tidyverse
  dplyr[filter, mutate, select, pull, arrange, group_by, summarize, n, between, row_number],
  lubridate[as_date, ymd, interval, `%within%`],
  tibble[tibble, as_tibble],
  stringr[str_replace],
  tidyr[unnest, pivot_longer],

  # File handling with arrow (Apache Arrow)
  arrow[read_parquet, write_parquet, Schema, schema],

  # Spatial data handling
  sf[st_read, st_drop_geometry, st_as_sf, st_bbox, st_coordinates, st_as_sfc, st_crs],
  terra[rast, values, global, crs, project, ext, as.polygons],

  # Basic utilities
  utils[head, tail],
  jsonlite[fromJSON, toJSON],

  # Color palettes
  viridisLite[magma],
  RColorBrewer[brewer.pal],
  grDevices[colorRampPalette],

  # Local dependencies
  app/logic/rtbm/rtbm_data_handlers[load_bird_species_info, load_parquet_data],
)

# Initialize bird species data
bird_spp_info <- load_bird_species_info()
species_choices <- bird_spp_info$common_name

#' Real-time Bird Monitoring UI Module
#'
#' @param id The module ID
#' @param i18n Internationalization function
#'
#' @return A Shiny UI definition
#' @export
rtbm_app_ui <- function(id, i18n) {
  ns <- NS(id)

  # Create base layout using htmltools
  base_layout <- div(
    class = "rtbm-container container-fluid p-3",
    div(
      class = "row g-3",
      # Sidebar Toggle Button (visible on mobile)
      div(
        class = "col-12 d-md-none mb-2",
        actionButton(
          inputId = ns("toggleSidebar"),
          label = "Toggle Controls",
          icon = icon("bars"),
          class = "btn btn-secondary w-100"
        )
      ),
      # Control Panel Sidebar
      div(
        id = ns("sidebarCol"),
        class = "col-md-3 sidebar-column",
        div(
          class = "control-panel card h-100",
          div(
            class = "card-header d-flex justify-content-between align-items-center",
            span("Bird Observation Controls"),
            # Desktop toggle button (smaller version)
            actionButton(
              inputId = ns("collapseSidebar"),
              label = NULL,
              icon = icon("chevron-left"),
              class = "btn btn-sm btn-outline-secondary collapse-sidebar-btn"
            )
          ),
          div(
            class = "card-body overflow-auto",
            # Current date display
            uiOutput(ns("currentDateDisplay")),
            # Date range picker
            dateRangeInput(
              inputId = ns("dateRange"),
              label = "Select Date Range",
              start = Sys.Date() - 30,
              end = Sys.Date(),
              min = "2010-01-01",
              max = Sys.Date(),
              format = "yyyy-mm-dd",
              startview = "month",
              weekstart = 1,
              separator = " to ",
              language = "en"
            ),
            # Species picker
            pickerInput(
              inputId = ns("speciesPicker"),
              label = "Select Species",
              choices = bird_spp_info$common_name,
              selected = bird_spp_info$common_name[1],
              multiple = FALSE,
              options = list(
                `live-search` = TRUE,
                size = 10,
                `actions-box` = TRUE
              )
            ),
            # Actions
            actionButton(
              inputId = ns("loadData"),
              label = "Load Data",
              icon = icon("refresh"),
              class = "btn btn-primary btn-block mt-3 mb-3 w-100"
            ),
            # Status message
            uiOutput(ns("statusMsg")),
            hr(),
            # Date slider with more height for better display
            uiOutput(ns("dateSlider"))
          )
        )
      ),
      # Collapsed sidebar state - only shows expand button
      div(
        id = ns("collapsedSidebar"),
        class = "col-auto sidebar-collapsed d-none",
        actionButton(
          inputId = ns("expandSidebar"),
          label = NULL,
          icon = icon("chevron-right"),
          class = "btn btn-secondary expand-sidebar-btn"
        )
      ),
      # Map Column - will expand when sidebar collapses
      div(
        id = ns("mapCol"),
        class = "col-md-9 map-column",
        leafletOutput(ns("rasterMap"), height = "800px")
      )
    )
  )

  # Wrap everything in tagList with styles
  tagList(
    # Include CSS resources
    tags$head(
      # Global styles (with RTBM styles now integrated into main.scss)
      tags$link(rel = "stylesheet", type = "text/css", href = "styles/main.css")
    ),
    useShinyjs(), # Initialize shinyjs
    base_layout
  )
}

#' Real-time Bird Monitoring Server Module
#'
#' @param id The module ID
#' @param tab_selected Reactive expression for tab selection
#'
#' @return A Shiny server function
#' @export
rtbm_app_server <- function(id, tab_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load Finland border for the map
    get_finland_border <- function() {
      # Try to load Finland border shapefile
      finland_file <- "app/data/rtbm/finland_border.geojson"

      if (file.exists(finland_file)) {
        tryCatch(
          {
            # Read border file
            finland <- st_read(finland_file, quiet = TRUE)
            return(finland)
          },
          error = function(e) {
            # If reading fails, create a bounding box
            print("Error reading Finland border file, using a bounding box instead")
            create_finland_bbox()
          }
        )
      } else {
        print("Finland border file not found, using a bounding box instead")
        create_finland_bbox()
      }
    }

    # Create a simple bounding box for Finland as fallback
    create_finland_bbox <- function() {
      # Create a bounding box for Finland as a fallback
      bbox <- st_bbox(c(xmin = 19, xmax = 32, ymin = 59, ymax = 71), crs = st_crs(4326))
      bbox_sf <- st_as_sfc(bbox)
      return(bbox_sf)
    }

    # Load Finland border
    finland_border <- get_finland_border()

    # Reactive values for data storage
    available_dates <- reactiveVal(NULL)
    current_date <- reactiveVal(NULL)
    current_frame_index <- reactiveVal(1)
    species_data <- reactiveVal(NULL)

    # Animation controls
    animation_speed <- reactiveVal(1000) # 1 second between frames
    animation_running <- reactiveVal(FALSE)
    animation_last_step <- reactiveVal(Sys.time()) # Track when the last step occurred

    # Sidebar state management
    sidebar_expanded <- reactiveVal(TRUE) # Start expanded

    # Toggle sidebar collapse/expand
    observeEvent(input$toggleSidebar, {
      # Toggle the sidebar visibility
      toggleClass(id = "sidebarCol", class = "d-none")
    })

    # Collapse sidebar button
    observeEvent(input$collapseSidebar, {
      addClass(id = "sidebarCol", class = "d-none")
      removeClass(id = "collapsedSidebar", class = "d-none")
      removeClass(id = "mapCol", class = "col-md-9")
      addClass(id = "mapCol", class = "col-md-11")
    })

    # Expand sidebar button
    observeEvent(input$expandSidebar, {
      removeClass(id = "sidebarCol", class = "d-none")
      addClass(id = "collapsedSidebar", class = "d-none")
      removeClass(id = "mapCol", class = "col-md-11")
      addClass(id = "mapCol", class = "col-md-9")
    })

    # Create flags to track if legend and info card are already added
    legend_added <- reactiveVal(FALSE)
    info_card_added <- reactiveVal(FALSE)

    # Generate date sequence based on selected range
    date_sequence <- reactive({
      req(input$dateRange)
      start_date <- as_date(input$dateRange[1])
      end_date <- as_date(input$dateRange[2])

      # Create date sequence
      seq.Date(
        from = start_date,
        to = end_date,
        by = "day"
      )
    })

    # Display current date in header
    output$currentDateDisplay <- renderUI({
      if (!is.null(current_date())) {
        span(
          class = "current-date",
          format_date_for_display(current_date())
        )
      }
    })

    # Format date for display
    format_date_for_display <- function(date) {
      if (is.null(date)) {
        return("")
      }

      # Handle various date formats
      tryCatch(
        {
          # First check if it's already a Date object
          if (inherits(date, "Date")) {
            return(format(date, "%b %d, %Y"))
          }

          # Try to parse as ISO date
          parsed_date <- try(as.Date(date), silent = TRUE)
          if (!inherits(parsed_date, "try-error") && !is.na(parsed_date)) {
            return(format(parsed_date, "%b %d, %Y"))
          }

          # If all else fails, return as is
          return(as.character(date))
        },
        error = function(e) {
          # Fallback for any errors
          return(as.character(date))
        }
      )
    }

    # Format multiple dates for display
    format_dates_for_display <- function(dates) {
      if (is.null(dates) || length(dates) == 0) {
        return(character(0))
      }
      vapply(dates, format_date_for_display, FUN.VALUE = character(1))
    }

    # Update map with a specific frame
    update_map_with_frame <- function(frame_index) {
      # Make sure we have required data
      if (is.null(available_dates()) || is.null(species_data())) {
        print("No dates or species data available")
        return(FALSE)
      }

      # Use tryCatch around the entire function to prevent any unexpected errors
      tryCatch(
        {
          # Convert frame_index to integer if needed
          frame_index <- as.integer(frame_index)

          # Safe print function to avoid formatting issues
          safe_print <- function(...) {
            args <- list(...)
            msg <- paste0(args, collapse = "")
            cat(msg, "\n")
          }

          safe_print("Updating map with frame index: ", frame_index)
          safe_print("Total available dates: ", length(available_dates()))

          # Validate frame index
          if (frame_index <= 0 || frame_index > length(available_dates())) {
            safe_print("Invalid frame index: ", frame_index)
            return(FALSE)
          }

          # Get the current date and data path (with validation)
          date <- available_dates()[[frame_index]]
          if (is.null(date)) {
            safe_print("Date is NULL for index: ", frame_index)
            return(FALSE)
          }

          # Ensure date is properly formatted
          if (!inherits(date, "Date")) {
            date <- as.Date(date)
          }

          scientific_name <- species_data()$scientific_name
          if (is.null(scientific_name) || is.na(scientific_name) || scientific_name == "") {
            safe_print("Invalid scientific name")
            return(FALSE)
          }

          # Format the date for file path
          date_str <- format(date, "%Y-%m-%d")

          # Construct path to parquet file
          parquet_path <- file.path(
            "app/data/rtbm/parquet",
            paste0("species=", scientific_name),
            paste0("date=", date_str, ".parquet")
          )

          safe_print("Looking for parquet file: ", parquet_path)

          # Check if file exists
          if (!file.exists(parquet_path)) {
            safe_print("Parquet file not found: ", parquet_path)

            output$statusMsg <- renderUI({
              div(
                class = "alert alert-warning",
                role = "alert",
                HTML(paste0("Data file not found for date: <strong>", date_str, "</strong>"))
              )
            })

            return(FALSE)
          }

          # Update current date
          current_date(date)
          safe_print("Current date set to: ", format(date, "%Y-%m-%d"))

          # Update map
          output$rasterMap <- renderLeaflet({
            # Read the parquet file directly
            tryCatch(
              {
                safe_print("Reading parquet file")
                points_data <- read_parquet(parquet_path)

                safe_print("Loaded parquet file with ", nrow(points_data), " points")

                if (ncol(points_data) > 0) {
                  safe_print("Columns in parquet: ", paste(colnames(points_data), collapse = ", "))
                }

                # Base map with a more neutral style
                m <- leaflet() |>
                  addProviderTiles("CartoDB.Positron") |>
                  # Set view to focus on southern Finland where most observations are
                  setView(lng = 25.0, lat = 62.0, zoom = 6)

                # Add Finland border if available
                if (!is.null(finland_border)) {
                  tryCatch(
                    {
                      m <- m |>
                        addPolylines(
                          data = finland_border,
                          color = "#FF6B6B",
                          weight = 2,
                          opacity = 0.8,
                          group = "Finland Border"
                        )
                    },
                    error = function(e) {
                      safe_print("Error adding Finland border: ", e$message)
                      # Continue without the border
                    }
                  )
                }

                # Only add bird data if we have points
                if (nrow(points_data) > 0) {
                  # Get column names
                  coord_cols <- c("longitude", "latitude", "intensity")

                  # Check if we need to rename columns
                  if (all(coord_cols %in% colnames(points_data))) {
                    # Debug intensities
                    safe_print(
                      "Intensity range: ",
                      min(points_data$intensity, na.rm = TRUE), " to ",
                      max(points_data$intensity, na.rm = TRUE)
                    )

                    # Use YlGnBu colormap from RColorBrewer (from CES module) instead of magma
                    # Create a sequential yellow-green-blue color palette
                    ylgnbu_colors <- colorRampPalette(brewer.pal(9, "YlGnBu"))(20)

                    # Create color function for intensity values
                    intensity_min <- min(points_data$intensity, na.rm = TRUE)
                    intensity_max <- max(points_data$intensity, na.rm = TRUE)

                    # Make sure we have a valid domain even when all values are the same
                    if (intensity_min == intensity_max) {
                      domain <- c(intensity_min, intensity_min + 0.000001)
                    } else {
                      domain <- c(intensity_min, intensity_max)
                    }

                    base_pal <- colorNumeric(ylgnbu_colors, domain = domain, na.color = NA)
                    pal_na <- function(x) {
                      col <- base_pal(x)
                      col[is.na(col)] <- "#00000000" # Transparent for NA values
                      col
                    }

                    # Add heatmap with YlGnBu colormap - confined to Finland's boundaries
                    m <- m |>
                      addHeatmap(
                        data = points_data,
                        lng = ~longitude,
                        lat = ~latitude,
                        intensity = ~ intensity * 4.0,
                        blur = 18,
                        max = intensity_max * 4.0 * 0.8,
                        radius = 15,
                        minOpacity = 0.7,
                        gradient = rev(ylgnbu_colors),
                        group = "Heat Map"
                      )

                    # Add layer controls with Heat Map as the only visualization option
                    m <- m |>
                      addLayersControl(
                        baseGroups = c("Base Map"),
                        overlayGroups = c("Finland Border", "Heat Map"),
                        options = layersControlOptions(collapsed = FALSE)
                      )

                    # Add legend with enhanced visualization
                    m <- m |> addLegend(
                      position = "bottomright",
                      pal = base_pal,
                      values = domain,
                      title = "Observation intensity",
                      opacity = 1.0 # Full opacity for better visibility
                    )

                    # Always re-add the bird info card if it's supposed to be there
                    if (info_card_added() &&
                      !is.null(input$speciesPicker) &&
                      !is.null(photo_url()) &&
                      !is.null(scientific_name()) &&
                      !is.null(wiki_link()) &&
                      !is.null(song_url())) {
                      # Create bird info card HTML with inline styles
                      bird_info_html <- HTML(paste0(
                        "<div style='background-color: white; padding: 10px; border-radius: 4px; border: 1px solid #dee2e6; width: 220px; box-shadow: 0 0.125rem 0.25rem rgba(0, 0, 0, 0.075);'>",
                        "<h5>", input$speciesPicker, "</h5>",
                        "<p><em><a href='", wiki_link(), "' target='_blank'>", scientific_name(), "</a></em></p>",
                        "<div style='text-align: center;'>",
                        "<img src='", photo_url(), "' alt='", input$speciesPicker, "' style='width: 200px;'>",
                        "</div>",
                        "<div style='margin-top: 10px;'>",
                        "<audio controls style='width: 100%;'>",
                        "<source src='", song_url(), "' type='audio/mp3'>",
                        "Your browser does not support the audio element.",
                        "</audio>",
                        "</div>",
                        "</div>"
                      ))

                      # Add the bird info card directly to the map
                      m <- m |> addControl(
                        html = bird_info_html,
                        position = "topleft",
                        layerId = "bird-info-card"
                      )
                    }

                    # Log success
                    safe_print("Successfully added ", nrow(points_data), " observation points to map")
                  } else {
                    safe_print("WARNING: Expected columns not found in parquet file")
                    safe_print("Available columns: ", paste(colnames(points_data), collapse = ", "))
                  }
                } else {
                  safe_print("No data points available for visualization")
                }

                # Add date display to map
                date_to_display <- tryCatch(
                  {
                    format_date_for_display(current_date())
                  },
                  error = function(e) {
                    safe_print("Error formatting date: ", e$message)
                    # Fallback to basic format
                    if (!is.null(current_date())) {
                      as.character(current_date())
                    } else {
                      "Unknown date"
                    }
                  }
                )

                m <- m |> addControl(
                  html = HTML(paste0(
                    "<div class='map-date-display'>",
                    "<strong>Date:</strong> ", date_to_display,
                    "</div>"
                  )),
                  position = "bottomleft"
                )

                return(m)
              },
              error = function(e) {
                safe_print("Error updating map: ", e$message)

                # Return a basic map if there's an error
                leaflet() |>
                  addProviderTiles("CartoDB.Positron") |>
                  # Set view to focus on southern Finland where most observations are
                  setView(lng = 25.0, lat = 62.0, zoom = 6) |>
                  addControl(
                    html = HTML(paste0(
                      "<div class='alert alert-danger'>",
                      "Error loading data: ", e$message,
                      "</div>"
                    )),
                    position = "topright"
                  )
              }
            )
          })

          return(TRUE)
        },
        error = function(e) {
          # Handle any unexpected errors
          cat("Error in update_map_with_frame: ", e$message, "\n")
          return(FALSE)
        }
      )
    }

    # Get Finnish name for URL construction
    finnish_name <- reactive({
      req(input$speciesPicker)
      fn <- bird_spp_info |>
        filter(common_name == input$speciesPicker) |>
        pull(finnish_name)
      print(fn)
      if (length(fn) == 0) {
        return(NULL)
      }
      fn
    })

    # Get photo URL for display
    photo_url <- reactive({
      req(input$speciesPicker)
      url <- bird_spp_info |>
        filter(common_name == input$speciesPicker) |>
        pull(photo_url)
      if (length(url) == 0 || url == "") {
        return(NULL)
      }
      url
    })

    # 3) Common name (English) - directly from input
    common_name <- reactive({
      req(input$speciesPicker)
      input$speciesPicker
    })

    # 4) Scientific name
    scientific_name <- reactive({
      req(input$speciesPicker)
      sn <- bird_spp_info |>
        filter(common_name == input$speciesPicker) |>
        pull(scientific_name)
      if (length(sn) == 0) {
        return(NULL)
      }
      sn
    })

    # 4b) Wiki link (for the hyperlink on the scientific name)
    wiki_link <- reactive({
      req(input$speciesPicker)
      wl <- bird_spp_info |>
        filter(common_name == input$speciesPicker) |>
        pull(wiki_link)
      if (length(wl) == 0 || wl == "") {
        return(NULL)
      }
      wl
    })

    # 5) Song URL
    song_url <- reactive({
      req(input$speciesPicker)
      s_url <- bird_spp_info |>
        filter(common_name == input$speciesPicker) |>
        pull(song_url)
      if (length(s_url) == 0 || s_url == "") {
        return(NULL)
      }
      s_url
    })

    # Process data for all dates in the selected range
    process_all_dates <- function() {
      req(input$dateRange, input$speciesPicker)

      start_date <- as_date(input$dateRange[1])
      end_date <- as_date(input$dateRange[2])
      species <- input$speciesPicker

      # Find scientific name for the selected species
      scientific_name <- bird_spp_info |>
        filter(common_name == species) |>
        pull(scientific_name)

      if (length(scientific_name) == 0) {
        output$statusMsg <- renderUI({
          div(
            class = "alert alert-danger",
            role = "alert",
            "Error: Could not find scientific name for the selected species."
          )
        })
        return(FALSE)
      }

      # Update status message
      output$statusMsg <- renderUI({
        div(
          class = "alert alert-info",
          role = "alert",
          "Loading data... This may take a few moments, especially if new data needs to be downloaded."
        )
      })

      # Use withProgress to show progress
      result <- NULL
      withProgress(
        message = "Processing data",
        value = 0,
        {
          # Load data using the parquet-based approach
          incProgress(0.2, detail = "Fetching observation data")
          result <- load_parquet_data(
            scientific_name = scientific_name,
            start_date = start_date,
            end_date = end_date
          )

          incProgress(0.3, detail = "Processing observations")

          if (is.null(result$data) || nrow(result$data) == 0 || length(result$dates) == 0) {
            output$statusMsg <- renderUI({
              div(
                class = "alert alert-warning",
                role = "alert",
                "No data available for the selected species and date range. Try a different date range or species."
              )
            })
            return(FALSE)
          }

          # Store dates and data paths
          available_dates(result$dates)
          species_data(list(
            scientific_name = scientific_name,
            data_paths = result$data_paths
          ))

          # Initialize with the first date
          if (length(result$dates) > 0) {
            current_date(result$dates[1])
            current_frame_index(1)
          }

          # Update slider with new date range
          dates <- available_dates()
          date_strings <- sapply(dates, format_date_for_display)
          date_mapping <- setNames(seq_along(dates), date_strings)
          session$userData$date_mapping <- date_mapping
          session$userData$dates <- dates

          # Update status message
          output$statusMsg <- renderUI({
            div(
              class = "alert alert-success",
              role = "alert",
              paste0(
                "Loaded data for ", length(result$dates), " dates. ",
                "Use the timeline slider or animation controls to view changes over time."
              )
            )
          })

          # Update map with the first frame
          update_map_with_frame(1)
        }
      )

      return(TRUE)
    }

    # Clear cache at midnight to get fresh data
    # observe({
    #   invalidateLater(calculate_seconds_until_midnight())
    #   Update local cache when time crosses midnight
    #   update_local_cache(days_back = 14)  # Temporarily disabled
    # })

    # Create a custom date slider UI using bslib components
    output$dateSlider <- renderUI({
      dates <- available_dates()
      if (is.null(dates) || length(dates) == 0) {
        return(NULL)
      }

      # Get the date strings for display
      date_strings <- sapply(dates, format_date_for_display)

      # Create a mapping from index to date for the slider
      date_mapping <- setNames(seq_along(dates), date_strings)

      # Store the mapping in session data for server access
      session$userData$date_mapping <- date_mapping
      session$userData$dates <- dates

      tagList(
        div(
          class = "date-slider-container",

          # Use sliderInput with custom labeling
          sliderInput(
            inputId = ns("date_slider_index"),
            label = "Select Date:",
            min = 1,
            max = length(dates),
            value = 1,
            step = 1,
            width = "100%",
            ticks = FALSE
          ),

          # Add text display for the currently selected date
          div(
            class = "date-display text-center my-2",
            textOutput(ns("selected_date_display"))
          ),

          # Animation controls - only shown after data is loaded
          div(
            class = "animation-controls mt-3",
            # Play/Pause Button (centered)
            div(
              class = "d-flex justify-content-center",
              uiOutput(ns("playPauseButton"))
            )
          )
        )
      )
    })

    # Display the selected date based on the slider index
    output$selected_date_display <- renderText({
      req(input$date_slider_index, session$userData$dates)

      # Get the selected date using the index
      selected_date <- session$userData$dates[input$date_slider_index]
      format_date_for_display(selected_date)
    })

    # Handle date slider changes
    observeEvent(input$date_slider_index, {
      req(session$userData$dates)

      # Get the actual date from the index
      selected_date <- session$userData$dates[input$date_slider_index]
      selected_date_str <- format_date_for_display(selected_date)

      # Update current date when slider changes
      current_date(selected_date)

      # Check if we have species data
      if (!is.null(species_data()) && !is.null(input$date_slider_index)) {
        # Update map with the selected date's frame
        update_map_with_frame(input$date_slider_index)

        # Re-add bird info card after this map update
        if (info_card_added() &&
          !is.null(input$speciesPicker) &&
          !is.null(photo_url()) &&
          !is.null(scientific_name()) &&
          !is.null(wiki_link()) &&
          !is.null(song_url())) {
          # Create bird info card using our reusable function
          bird_info_html <- create_bird_info_card(
            species_name = input$speciesPicker,
            scientific_name = scientific_name(),
            wiki_url = wiki_link(),
            photo_url = photo_url(),
            song_url = song_url()
          )

          # Add the bird info card directly to the map
          leafletProxy(ns("rasterMap")) |>
            removeControl(layerId = "bird-info-card") |>
            addControl(
              html = bird_info_html,
              position = "topleft",
              layerId = "bird-info-card"
            )
        }
      }
    })

    # Play/Pause button UI
    output$playPauseButton <- renderUI({
      actionButton(
        inputId = ns("animateControl"),
        label = if (animation_running()) "Pause" else "Play",
        icon = if (animation_running()) icon("pause") else icon("play"),
        class = "btn-primary btn-lg",
        width = "100%"
      )
    })

    # Handle play button
    observeEvent(input$play_animation, {
      animation_running(TRUE)
      # Start the animation immediately
      animation_step()
    })

    # Handle pause button
    observeEvent(input$pause_animation, {
      animation_running(FALSE)
    })

    # Handle animation control button
    observeEvent(input$animateControl, {
      # Toggle animation state
      animation_running(!animation_running())

      # Reset the last step time when starting animation
      if (animation_running()) {
        # Set to a time in the past to ensure first step happens immediately
        animation_last_step(Sys.time() - 3)
      }

      # Force UI update for button
      output$playPauseButton <- renderUI({
        actionButton(
          inputId = ns("animateControl"),
          label = if (animation_running()) "Pause" else "Play",
          icon = if (animation_running()) icon("pause") else icon("play"),
          class = "btn-primary btn-lg",
          width = "100%"
        )
      })

      # Start animation if now playing
      if (animation_running()) {
        animation_step()
      }
    })

    # Function to handle a single animation step
    animation_step <- function() {
      req(animation_running())

      # Get dates and current index
      dates <- available_dates()
      if (is.null(dates) || length(dates) == 0) {
        return(NULL)
      }

      # Get the current slider index
      current_idx <- isolate(input$date_slider_index)

      # Calculate next index (with loop back to beginning)
      next_idx <- if (current_idx >= length(dates)) 1 else current_idx + 1

      # Update the slider
      updateSliderInput(session, "date_slider_index", value = next_idx)

      # Schedule next step if animation is still running
      if (isolate(animation_running())) {
        invalidateLater(animation_speed())
      }
    }

    # Observer to handle animation steps
    observe({
      req(animation_running())

      # Only proceed if enough time has passed since last step
      current_time <- Sys.time()
      last_step_time <- animation_last_step()
      time_diff <- as.numeric(difftime(current_time, last_step_time, units = "secs"))

      if (time_diff >= (animation_speed() / 1000)) {
        # Update the last step time
        animation_last_step(current_time)

        # Execute animation step
        animation_step()
      } else {
        # Schedule check again after a short delay
        invalidateLater(100)
      }
    })

    # Function to create bird info card HTML with inline styles
    create_bird_info_card <- function(species_name, scientific_name, wiki_url, photo_url, song_url) {
      HTML(paste0(
        "<div style='background-color: white; padding: 10px; border-radius: 4px; border: 1px solid #dee2e6; width: 220px; box-shadow: 0 0.125rem 0.25rem rgba(0, 0, 0, 0.075);'>",
        "<h5>", species_name, "</h5>",
        "<p><em><a href='", wiki_url, "' target='_blank'>", scientific_name, "</a></em></p>",
        "<div style='text-align: center;'>",
        "<img src='", photo_url, "' alt='", species_name, "' style='width: 200px;'>",
        "</div>",
        "<div style='margin-top: 10px;'>",
        "<audio controls style='width: 100%;'>",
        "<source src='", song_url, "' type='audio/mp3'>",
        "Your browser does not support the audio element.",
        "</audio>",
        "</div>",
        "</div>"
      ))
    }

    # Function to add bird info card to map
    add_bird_info_card <- function() {
      # Only proceed if we have all the required data
      req(
        input$speciesPicker,
        photo_url(),
        scientific_name(),
        wiki_link(),
        song_url()
      )

      # Create bird info card using our reusable function
      bird_info_html <- create_bird_info_card(
        species_name = input$speciesPicker,
        scientific_name = scientific_name(),
        wiki_url = wiki_link(),
        photo_url = photo_url(),
        song_url = song_url()
      )

      # Add the card to the map, replacing any existing one
      leafletProxy(ns("rasterMap")) |>
        removeControl(layerId = "bird-info-card") |>
        addControl(
          html = bird_info_html,
          position = "topleft",
          layerId = "bird-info-card"
        )

      # Update flag to indicate the info card is added
      info_card_added(TRUE)
    }

    # Add observer to ensure bird info card persists after any map operations
    observe({
      # Watch for map changes that might affect the bird info card
      input$rasterMap_zoom
      input$rasterMap_bounds
      input$loadData

      # Only proceed if we should show the bird info card
      if (info_card_added() &&
        !is.null(input$speciesPicker) &&
        !is.null(photo_url()) &&
        !is.null(scientific_name()) &&
        !is.null(wiki_link()) &&
        !is.null(song_url())) {
        # Add a small delay to ensure this runs after other map operations
        delay(200, {
          # Add bird info card to the map
          add_bird_info_card()
        })
      }
    })

    # Observe species selection and update the info card on the map
    observeEvent(input$speciesPicker, {
      # Only proceed if we have all required data
      req(photo_url(), scientific_name(), wiki_link(), song_url())

      # Add bird info card to the map
      add_bird_info_card()

      # Also update the map frame if we have data available
      if (!is.null(current_frame_index())) {
        update_map_with_frame(current_frame_index())
      }
    })

    # Changed from reactive to eventReactive to only load data when the button is clicked
    raster_data <- eventReactive(input$loadData, {
      # Reset legend flag when loading new data, but keep info card flag
      legend_added(FALSE)
      # Do NOT reset info_card_added flag here - important for persistence

      # Process all dates and prepare data
      success <- process_all_dates()
      print(paste0("Data processing complete. Success: ", success))

      if (success) {
        # Show the first frame
        print("Displaying first frame")
        update_map_with_frame(1)
      } else {
        # Clear the map if no data
        print("No data available - clearing the map")
        leafletProxy(ns("rasterMap")) |>
          clearImages() |>
          clearControls()
      }
    })

    # Observe button click to trigger initial data load
    observeEvent(input$loadData, {
      raster_data()

      # Add a slight delay and then re-add both legend and bird info card
      delay(500, {
        # Re-add the legend
        if (legend_added()) {
          # We need to recreate the legend with the same parameters
          # Assuming points_data is available from the most recent update
          leafletProxy(ns("rasterMap")) |>
            clearControls() |> # Clear existing controls
            addLegend(
              position = "bottomright",
              pal = colorNumeric(palette = "viridis", domain = c(0, 1))(seq(0, 1, length.out = 5)),
              values = seq(0, 1, length.out = 5),
              title = "Observation Intensity",
              opacity = 0.7
            )
        }

        # Re-add the bird info card if needed
        if (info_card_added()) {
          # Make sure we have all the required bird info available
          if (!is.null(input$speciesPicker) &&
            !is.null(photo_url()) &&
            !is.null(scientific_name()) &&
            !is.null(wiki_link()) &&
            !is.null(song_url())) {
            # Add bird info card to the map
            add_bird_info_card()
          }
        }
      })
    })

    # Base leaflet map
    output$rasterMap <- renderLeaflet({
      # Create the base map
      m <- leaflet() |>
        addProviderTiles("CartoDB.Positron") |>
        # Set view to focus on southern Finland where most observations are
        setView(lng = 25.0, lat = 62.0, zoom = 6) |>
        addControl(
          html = "<div id='hover-info' class='map-hover-display d-none'></div>",
          position = "topright",
          layerId = "hover-info-control"
        )

      # If we already have the bird info card, add it to the initial map
      if (info_card_added() && !is.null(input$speciesPicker) &&
        !is.null(photo_url()) && !is.null(scientific_name()) &&
        !is.null(wiki_link()) && !is.null(song_url())) {
        # Add bird info card to the map
        add_bird_info_card()
      }

      return(m)
    })
  })
}

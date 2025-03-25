box::use(
  # HTML structure (htmltools)
  htmltools[
    a, div, em, HTML, img, p, renderTags, br, h1, h2, h3, h4, h5, h6,
    strong, tagList, tags, tagQuery, span, hr, includeCSS, includeScript
  ],

  # Shiny fundamentals
  shiny[
    moduleServer, NS, renderUI, tags, icon, div, observe, observeEvent,
    isolate, reactiveVal, reactiveValues, req, reactive, renderPlot,
    invalidateLater, updateSliderInput, withProgress, incProgress, removeUI,
    actionButton, column, fluidRow, selectInput, sliderInput, tagList,
    insertUI, uiOutput, h4, h5, h6, hr, p, pre, plotOutput, dateRangeInput,
    textOutput, renderText, updateDateRangeInput, updateSelectInput, eventReactive
  ],

  # UI components
  bslib[card, card_header, card_body, card_footer, tooltip, value_box, layout_column_wrap],
  shinyWidgets[sliderTextInput, pickerInput, updateSliderTextInput, progressBar, updateProgressBar],
  shinyjs[useShinyjs, runjs, hide, show, toggle, addClass, removeClass, toggleClass],
  htmlwidgets[onRender],

  # Interactive map
  leaflet[
    leaflet, addTiles, setView, addProviderTiles, clearShapes, addCircles,
    addCircleMarkers, addControl, clearControls, addLegend, addPolylines,
    layersControlOptions, addLayersControl, leafletOutput, renderLeaflet, leafletProxy,
    addRasterImage, clearImages, colorNumeric, addMarkers, clearGroup, makeIcon,
    addRectangles, removeControl
  ],
  leaflet.extras[addHeatmap],

  # Data manipulation
  dplyr[filter, mutate, select, pull, arrange, group_by, summarize, n, between, row_number],
  magrittr[`%>%`],
  lubridate[as_date, ymd, `%within%`, interval],
  tibble[tibble, as_tibble],
  stringr[str_replace],
  tidyr[unnest, pivot_longer],
  arrow[read_parquet, write_parquet, Schema, schema],
  jsonlite[fromJSON, toJSON],

  # Raster and spatial handling
  sf[st_read, st_drop_geometry, st_as_sf, st_bbox, st_coordinates, st_as_sfc, st_crs],
  terra[rast, values, global, crs, project, ext, as.polygons],
  fs[dir_create, path, path_package],

  # Stats and utilities
  stats[na.omit, quantile, median, sd, var],
  utils[head, tail, str, capture.output],

  # Local dependencies
  ./rtbm_data_preprocessing[
    load_bird_species, update_local_cache, load_species_data
  ],
)

#' Load and prepare bird species information
#' @noRd
load_bird_species <- function() {
  bird_info_url <- "https://bird-photos.a3s.fi/bird_info.json"
  bird_info <- fromJSON(bird_info_url)

  # Process the nested JSON structure
  bird_df <- do.call(rbind, lapply(names(bird_info), function(species_key) {
    info <- bird_info[[species_key]]
    data.frame(
      common_name = info$common_name,
      scientific_name = info$scientific_name,
      finnish_name = info$finnish_name,
      photo_url = info$photo_url,
      wiki_link = info$wiki_link,
      song_url = info$song_url,
      stringsAsFactors = FALSE
    )
  }))

  # Convert to tibble and arrange by common name
  as_tibble(bird_df) |>
    arrange(common_name) |>
    mutate(
      scientific_name = stringr::str_replace(
        string = scientific_name,
        pattern = " ",
        replacement = "_"
      )
    )
}

# Initialize bird species data
bird_spp_info <- load_bird_species()
species_choices <- bird_spp_info$common_name

#' Calculate seconds until midnight for cache refresh
#' @noRd
calculate_seconds_until_midnight <- function() {
  current_time <- Sys.time()
  midnight <- as.POSIXct(format(current_time + 86400, "%Y-%m-%d 00:00:00"))
  as.numeric(difftime(midnight, current_time, units = "secs"))
}

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
            div(
              class = "date-slider-container",
              uiOutput(ns("dateSlider"))
            )
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
      # Import global styles first
      tags$link(rel = "stylesheet", type = "text/css", href = "styles/main.css"),
      # Then module-specific styles that only contain necessary overrides
      tags$link(rel = "stylesheet", type = "text/css", href = "view/rtbm/styles.css")
    ),
    shinyjs::useShinyjs(), # Initialize shinyjs
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
    animation_speed <- reactiveVal(1000) # milliseconds between frames
    animation_running <- reactiveVal(FALSE)

    # Sidebar state management
    sidebar_expanded <- reactiveVal(TRUE) # Start expanded

    # Handle sidebar toggle on mobile
    observeEvent(input$toggleSidebar, {
      shinyjs::toggleClass(id = "sidebarCol", class = "d-none")
    })

    # Handle sidebar collapse on desktop
    observeEvent(input$collapseSidebar, {
      sidebar_expanded(FALSE)
      shinyjs::addClass(id = "sidebarCol", class = "d-none")
      shinyjs::removeClass(id = "collapsedSidebar", class = "d-none")
      shinyjs::removeClass(id = "mapCol", class = "col-md-9")
      shinyjs::addClass(id = "mapCol", class = "col-md-11")
    })

    # Handle sidebar expand on desktop
    observeEvent(input$expandSidebar, {
      sidebar_expanded(TRUE)
      shinyjs::removeClass(id = "sidebarCol", class = "d-none")
      shinyjs::addClass(id = "collapsedSidebar", class = "d-none")
      shinyjs::removeClass(id = "mapCol", class = "col-md-11")
      shinyjs::addClass(id = "mapCol", class = "col-md-9")
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
                points_data <- arrow::read_parquet(parquet_path)

                safe_print("Loaded parquet file with ", nrow(points_data), " points")

                if (ncol(points_data) > 0) {
                  safe_print("Columns in parquet: ", paste(colnames(points_data), collapse = ", "))
                }

                # Base map with a more neutral style
                m <- leaflet() |>
                  addProviderTiles("CartoDB.Positron") |>
                  setView(lng = 25, lat = 65, zoom = 4)

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

                    # Create a color palette for intensity values
                    intensity_min <- min(points_data$intensity, na.rm = TRUE)
                    intensity_max <- max(points_data$intensity, na.rm = TRUE)

                    if (intensity_min == intensity_max) {
                      # If all values are the same, create a range to avoid domain error
                      domain <- c(intensity_min, intensity_min + 0.000001)
                    } else {
                      domain <- c(intensity_min, intensity_max)
                    }

                    pal <- colorNumeric(
                      palette = "viridis",
                      domain = domain
                    )

                    # Add visualization layers

                    # 1. Circle markers for better visibility at all zoom levels
                    m <- m |>
                      addCircleMarkers(
                        data = points_data,
                        lng = ~longitude,
                        lat = ~latitude,
                        radius = ~ pmin(8, 3 + intensity * 5), # Size based on intensity (capped)
                        color = "#000",
                        weight = 1,
                        fillColor = ~ pal(intensity),
                        fillOpacity = 0.7,
                        popup = ~ paste0("<strong>Intensity:</strong> ", round(intensity, 4)),
                        group = "Bird Markers"
                      )

                    # 2. Add heatmap for density visualization
                    if (requireNamespace("leaflet.extras", quietly = TRUE)) {
                      m <- m |>
                        leaflet.extras::addHeatmap(
                          data = points_data,
                          lng = ~longitude,
                          lat = ~latitude,
                          intensity = ~intensity,
                          blur = 15,
                          max = intensity_max,
                          radius = 10,
                          group = "Heat Map"
                        )
                    }

                    # 3. Add layer controls to switch between visualizations
                    m <- m |>
                      addLayersControl(
                        baseGroups = c("Base Map"),
                        overlayGroups = c("Finland Border", "Bird Markers", "Heat Map"),
                        options = layersControlOptions(collapsed = FALSE)
                      )

                    # 4. Add legend
                    m <- m |>
                      addLegend(
                        position = "bottomright",
                        pal = pal,
                        values = points_data$intensity,
                        title = "Observation intensity",
                        opacity = 0.7
                      )

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
                  setView(lng = 25, lat = 65, zoom = 4) |>
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
          result <- load_species_data(
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
          updateSliderTextInput(
            session = session,
            inputId = "date_slider",
            choices = format_dates_for_display(result$dates),
            selected = format_date_for_display(result$dates[1])
          )

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

    # Date slider for manual navigation
    output$dateSlider <- renderUI({
      req(available_dates())
      dates <- available_dates()

      if (length(dates) == 0) {
        return(NULL)
      }

      tagList(
        sliderTextInput(
          inputId = ns("date_slider"),
          label = "Select Date:",
          choices = sapply(dates, format_date_for_display),
          selected = format_date_for_display(dates[1]),
          grid = TRUE,
          animate = FALSE,
          width = "100%"
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
      if (length(dates) == 0) {
        return()
      }

      date_strings <- sapply(dates, format_date_for_display)
      current_idx <- which(date_strings == input$date_slider)

      if (length(current_idx) == 0 || current_idx < 1) {
        current_idx <- 1 # Default to first frame if not found
      }

      # Move to next date (or cycle back to beginning)
      next_idx <- if (current_idx < length(dates)) current_idx + 1 else 1
      next_date <- dates[next_idx]

      # Update slider to new date
      updateSliderTextInput(
        session = session,
        inputId = "date_slider",
        selected = format_date_for_display(next_date)
      )

      # Update map with the new frame
      update_map_with_frame(next_idx)

      # Schedule next animation step after a delay if still playing
      if (animation_running()) {
        invalidateLater(animation_speed()) # delay between frames
        observeEvent(invalidateLater(animation_speed()),
          {
            animation_step()
          },
          once = TRUE
        )
      }
    }

    # Clear cache at midnight to get fresh data
    observe({
      invalidateLater(calculate_seconds_until_midnight())
      # Update local cache when time crosses midnight
      # update_local_cache(days_back = 14)  # Temporarily disabled
    })

    # Handle manual date slider change
    observeEvent(input$date_slider, {
      req(available_dates())
      selected_date_str <- input$date_slider
      selected_date <- as_date(selected_date_str)

      # Update current date when slider changes
      current_date(selected_date)

      dates <- available_dates()
      date_strings <- sapply(dates, format_date_for_display)
      new_frame <- which(date_strings == selected_date_str)

      if (length(new_frame) > 0 && new_frame > 0) {
        update_map_with_frame(new_frame)
      }
    })

    # Changed from reactive to eventReactive to only load data when the button is clicked
    raster_data <- eventReactive(input$loadData, {
      # Reset flags when loading new data
      legend_added(FALSE)
      info_card_added(FALSE)

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

    # Base leaflet map
    output$rasterMap <- renderLeaflet({
      leaflet() |>
        addProviderTiles("CartoDB.Positron", layerId = "base_map") |>
        setView(lng = 25, lat = 65.5, zoom = 5) |>
        # Add empty controls that will be filled later
        addControl(
          html = "<div id='hover-info' class='map-hover-display d-none'></div>",
          position = "topright",
          layerId = "hover-info-control"
        )
    })

    # Initial status message prompting user action
    output$statusMsg <- renderUI({
      div(
        class = "alert alert-info",
        role = "alert",
        "Select a date range and species, then click 'Load Data' to view the distribution map. Use the date slider to navigate through time."
      )
    })

    # Observe button click to trigger initial data load
    observeEvent(input$loadData, {
      raster_data()
    })

    # Track map view changes
    observeEvent(input$rasterMap_zoom, {
      # Store current zoom level
      current_zoom <- input$rasterMap_zoom
      print(paste0("Zoom level changed to: ", current_zoom))

      # Just track zoom level but don't try to manipulate markers directly
      # Let the CSS solution handle consistent appearance
    })

    # Track map bounds changes
    observeEvent(input$rasterMap_bounds, {
      # Store current bounds
      current_bounds <- input$rasterMap_bounds
      print(paste0("Map bounds changed"))

      # This could be used to load more detailed data for the visible area
      # or adjust data display based on the current view
    })

    # Handle marker clicks
    observeEvent(input$rasterMap_marker_click, {
      click_data <- input$rasterMap_marker_click
      print(paste0("Marker clicked: ", click_data$id))

      # Extract marker ID from the layerId
      marker_id <- sub("marker_", "", click_data$id)

      # Additional click handling can be implemented here
      # For example, showing detailed information about the specific observation
    })

    # Handle map shape hover events
    observeEvent(input$rasterMap_shape_mouseover, {
      hover_data <- input$rasterMap_shape_mouseover

      if (!is.null(hover_data)) {
        # Show hover info with the observation value
        intensity_value <- if (is.numeric(hover_data$value)) {
          round(hover_data$value, 2)
        } else {
          "N/A" # Fallback for non-numeric values
        }

        leafletProxy(ns("rasterMap")) |>
          addControl(
            html = HTML(paste0(
              "<div class='map-hover-display'>",
              "<strong>Intensity:</strong> ", intensity_value,
              "</div>"
            )),
            position = "topright",
            layerId = "hover-info-control"
          )
      }
    })

    observeEvent(input$rasterMap_shape_mouseout, {
      # Hide or clear hover information
      leafletProxy(ns("rasterMap")) |>
        addControl(
          html = "<div class='map-hover-display d-none'></div>",
          position = "topright",
          layerId = "hover-info-control"
        )
    })

    # Handle animation control button
    observeEvent(input$animateControl, {
      # Toggle animation state
      animation_running(!animation_running())

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
  })
}

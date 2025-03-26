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
  shinyjs[useShinyjs, runjs, hide, show, toggle, addClass, removeClass, toggleClass, delay],
  htmlwidgets[onRender],

  # Interactive map
  leaflet[
    leaflet, addTiles, setView, addProviderTiles, clearShapes, addCircles,
    addCircleMarkers, addControl, clearControls, addLegend, addPolylines,
    layersControlOptions, addLayersControl, leafletOutput, renderLeaflet, leafletProxy,
    addRasterImage, clearImages, colorNumeric, addMarkers, clearGroup, makeIcon,
    addRectangles, removeControl, removeTiles
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
  . / rtbm_data_preprocessing[
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
                    m <- m |> addLegend(
                      position = "bottomright",
                      pal = pal,
                      values = points_data$intensity,
                      title = "Observation intensity",
                      opacity = 0.7
                    )

                    # Always re-add the bird info card if it's supposed to be there
                    if (info_card_added()) {
                      # Make sure we have all bird info available
                      if (!is.null(input$speciesPicker) && !is.null(photo_url()) && !is.null(scientific_name()) && !is.null(wiki_link()) && !is.null(song_url())) {
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

    # Process all dates in the date range
    process_all_dates <- function(scientific_name, common_name, start_date, end_date) {
      # Use withProgress to show a loading indicator
      withProgress(
        message = paste("Loading data for", common_name),
        value = 0,
        {
          # Reset reactive values
          available_dates(NULL)
          species_data(NULL)
          current_date(NULL)
          current_frame_index(NULL)

          incProgress(0.1, detail = "Checking for data files")

          # Load data for the species
          result <- load_species_data(
            scientific_name = scientific_name,
            start_date = start_date,
            end_date = end_date
          )

          incProgress(0.3, detail = "Processing observations")

          # Check if there was an error or no data is available
          if (result$error || is.null(result$data) || nrow(result$data) == 0 || length(result$dates) == 0) {
            # Show specific error message if available, otherwise a generic one
            output$statusMsg <- renderUI({
              div(
                class = "alert alert-warning",
                role = "alert",
                if (!is.null(result$error_message)) {
                  result$error_message
                } else {
                  "No data available for the selected species and date range. Try a different date range or species."
                }
              )
            })

            # Clear the map
            leafletProxy(ns("rasterMap")) |>
              clearImages() |>
              clearControls()

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

    # Clear cache at midnight to get fresh data
    observe({
      invalidateLater(calculate_seconds_until_midnight())
      # Update local cache when time crosses midnight
      # update_local_cache(days_back = 14)  # Temporarily disabled
    })

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

    # Observe date slider changes
    observeEvent(input$date_slider, {
      req(available_dates())
      selected_date_str <- input$date_slider
      selected_date <- as_date(selected_date_str)

      # Update current date when slider changes
      current_date(selected_date)

      dates <- available_dates()
      date_strings <- sapply(dates, format_date_for_display)
      new_frame <- which(date_strings == selected_date_str)

      if (length(new_frame) > 0 && !is.na(new_frame)) {
        current_frame_index(new_frame)
        update_map_with_frame(new_frame)

        # Re-add bird info card after this map update
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
          shinyjs::delay(100, {
            leafletProxy(ns("rasterMap")) |>
              removeControl(layerId = "bird-info-card") |>
              addControl(
                html = bird_info_html,
                position = "topleft",
                layerId = "bird-info-card"
              )
          })
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
        shinyjs::delay(200, {
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

          # Add bird info card to the map
          leafletProxy(ns("rasterMap")) |>
            removeControl(layerId = "bird-info-card") |>
            addControl(
              html = bird_info_html,
              position = "topleft",
              layerId = "bird-info-card"
            )
        })
      }
    })

    # Observe species selection and update the info card on the map
    observeEvent(input$speciesPicker, {
      # Only proceed if we have all required data
      req(photo_url(), scientific_name(), wiki_link(), song_url())

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

      # Add the bird info card to the map, replacing any existing one
      leafletProxy(ns("rasterMap")) |>
        removeControl(layerId = "bird-info-card") |>
        addControl(
          html = bird_info_html,
          position = "topleft",
          layerId = "bird-info-card"
        )

      # Update flag to indicate the info card is added
      info_card_added(TRUE)

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
      success <- process_all_dates(scientific_name = scientific_name(), common_name = common_name(), start_date = as_date(input$dateRange[1]), end_date = as_date(input$dateRange[2]))
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
      shinyjs::delay(500, {
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

            # Add bird info card back to the map
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
    })

    # Base leaflet map
    output$rasterMap <- renderLeaflet({
      # Create the base map
      m <- leaflet() |>
        addProviderTiles("CartoDB.Positron") |>
        setView(lng = 25, lat = 65.5, zoom = 4) |>
        addControl(
          html = "<div id='hover-info' class='map-hover-display d-none'></div>",
          position = "topright",
          layerId = "hover-info-control"
        )

      # If we already have the bird info card, add it to the initial map
      if (info_card_added() && !is.null(input$speciesPicker) &&
        !is.null(photo_url()) && !is.null(scientific_name()) &&
        !is.null(wiki_link()) && !is.null(song_url())) {
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

        # Add bird info card to the initial map
        m <- m |> addControl(
          html = bird_info_html,
          position = "topleft",
          layerId = "bird-info-card"
        )
      }

      return(m)
    })
  })
}

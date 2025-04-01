# External packages
box::use(
  # Shiny fundamentals and UI
  shiny[
    moduleServer, NS, renderUI, observe, observeEvent,
    isolate, reactiveVal, reactiveValues, req, reactive, renderPlot,
    invalidateLater, updateSliderInput, updateTextInput, withProgress, incProgress, removeUI,
    actionButton, column, fluidRow, selectInput, sliderInput,
    insertUI, uiOutput, renderText, updateDateRangeInput, updateSelectInput, eventReactive,
    icon, textInput, showNotification, verbatimTextOutput, renderPrint,
    dateRangeInput, textOutput
  ],

  # Base R functions needed
  stats[setNames, na.omit, quantile, median, sd, var],

  # HTML structure
  htmltools[
    a, div, em, HTML, img, p, br, h1, h2, h3, h4, h5, h6,
    strong, tagList, span, hr, tags, pre
  ],

  # Rhinoverse ecosystem UI components
  bslib[
    card, card_header, card_body, card_footer, tooltip, value_box, layout_column_wrap,
    page_fluid, nav_panel, nav_spacer, nav_menu, nav_item, nav, navset_bar,
    as_fill_carrier, as_fill_item
  ],

  # Modern Shiny components
  shinyWidgets[pickerInput, progressBar, updateProgressBar],
  shinyjs[useShinyjs, runjs, hide, show, toggle, addClass, removeClass, toggleClass, delay],

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

  # Basic utilities
  utils[head, tail],
  jsonlite[fromJSON, toJSON],
)

# Local modules
box::use(
  app/logic/rtbm/rtbm_data_handlers[load_bird_species_info, load_parquet_data],
  app/view/rtbm/rtbm_map[map_module_ui, map_module_server],
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
        map_module_ui(ns("map"))
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
          map_functions$update_map_with_frame(1)
        }
      )

      return(TRUE)
    }

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
        map_functions$update_map_with_frame(input$date_slider_index)
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
        map_functions$update_map_with_frame(1)
      } else {
        # Clear the map if no data
        print("No data available - clearing the map")
        leafletProxy(ns("map-rasterMap")) |>
          clearImages() |>
          clearControls()
      }
    })

    # Observe button click to trigger initial data load
    observeEvent(input$loadData, {
      raster_data()
    })

    # Initialize map module
    map_functions <- map_module_server(
      "map",
      finland_border = finland_border,
      current_date = current_date,
      species_data = species_data,
      selected_species = reactive(input$speciesPicker),
      photo_url = photo_url,
      scientific_name = scientific_name,
      wiki_link = wiki_link,
      song_url = song_url
    )
  })
}

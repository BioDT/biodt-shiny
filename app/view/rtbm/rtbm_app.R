box::use(
  # HTML structure (htmltools)
  htmltools[
    a, div, em, HTML, img, p, renderTags,
    strong, tagList, tags, tagQuery, span, hr
  ],

  # Reactive components (Shiny)
  shiny[
    NS, uiOutput, actionButton, dateRangeInput,
    moduleServer, observe, observeEvent,
    reactive, req, renderUI, eventReactive,
    invalidateLater, reactiveVal, sliderInput,
    icon, textOutput, renderText, isTruthy
  ],

  # Bootstrap components
  bslib[
    card, card_body, card_footer, card_header
  ],

  # Leaflet map components
  leaflet[
    leaflet, leafletOutput, leafletProxy, renderLeaflet,
    addProviderTiles, addTiles, setView, addControl,
    addLegend, addRasterImage, clearControls,
    clearImages, colorNumeric, clearShapes
  ],

  # Data manipulation
  dplyr[arrange, filter, mutate, pull, select, slice],
  stringr[str_detect],
  sf[st_crs, st_as_sf, st_sfc, st_sf],
  tibble[as_tibble],
  jsonlite[fromJSON],
  tidyjson[spread_all],
  lubridate[today, as_date, interval],
  httr2[request, req_perform, req_url_path, req_url_query, resp_status, resp_body_json],

  # File and data handling
  terra[rast],
  utils[download.file],
  stats[na.omit],
  raster[crs, projectRaster, raster, values, projection],
  tools[file_ext],
  grDevices[colorRampPalette],
  memoise[memoise, forget],

  # UI widgets
  shinyWidgets[pickerInput, sliderTextInput],

  # Color palettes
  viridisLite[magma, inferno],
)

#' Load and prepare bird species information
#' @noRd
load_bird_species <- function() {
  bird_info_url <- "https://bird-photos.a3s.fi/bird_info.json"
  bird_info <- fromJSON(bird_info_url)

  bird_info |>
    spread_all() |>
    as_tibble() |>
    select(-document.id) |>
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
      # Control Panel
      div(
        class = "col-md-3",
        div(
          class = "control-panel card",
          div(
            class = "card-body",
            # Current date display
            uiOutput(ns("currentDateDisplay")),
            hr(),
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
            hr(),
            # Status message
            uiOutput(ns("statusMsg")),
            hr(),
            # Date slider
            uiOutput(ns("dateSlider"))
          )
        )
      ),
      # Map Column
      div(
        class = "col-md-9",
        leafletOutput(ns("rasterMap"), height = "800px")
      )
    )
  )

  # Use tagQuery for dynamic modifications
  control_panel <- tagQuery(base_layout)$
    find(".control-panel")$
    append(
    # Status message container
    div(
      id = ns("statusMsgContainer"),
      class = "alert-container mt-3",
      `aria-live` = "polite",
      uiOutput(ns("statusMsg"))
    )
  )$allTags()

  # Wrap everything in tagList with styles
  tagList(
    # Include CSS resources
    tags$head(
      # Import global styles first
      tags$link(rel = "stylesheet", type = "text/css", href = "styles/main.css"),
      # Then module-specific styles that only contain necessary overrides
      tags$link(rel = "stylesheet", type = "text/css", href = "view/rtbm/styles.css"),
      # Custom CSS for rainfall animation style
      tags$style(HTML("
        .leaflet-weather-control {
          background-color: rgba(255, 255, 255, 0.8);
          padding: 10px;
          border-radius: 4px;
          box-shadow: 0 1px 5px rgba(0,0,0,0.4);
        }
        .time-controls {
          padding: 10px;
          background-color: #f8f9fa;
          border-radius: 4px;
        }
        .time-slider .js-range-slider {
          width: 100%;
        }
        .animation-buttons .btn {
          min-width: 80px;
        }
        .date-label {
          font-weight: bold;
          font-size: 1.2em;
          text-align: center;
          margin-bottom: 10px;
        }
        .rain-legend {
          background: linear-gradient(to right, #f0f9ff, #0077be, #00305a);
          height: 20px;
          border-radius: 4px;
          margin-top: 5px;
        }
        .rain-legend-labels {
          display: flex;
          justify-content: space-between;
          margin-top: 5px;
          font-size: 0.8em;
        }
      "))
    ),
    control_panel
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

    # Reactive values for data storage
    available_dates <- reactiveVal(NULL)
    frames_data <- reactiveVal(list())
    current_date <- reactive({
      req(input$date_slider, available_dates())
      selected_date_str <- input$date_slider
      selected_date <- as_date(selected_date_str)
      return(selected_date)
    })
    current_frame <- reactiveVal(1)

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
          format(current_date(), "%B %d, %Y")
        )
      }
    })

    # Date slider for manual navigation
    output$dateSlider <- renderUI({
      req(available_dates())
      dates <- available_dates()

      if (length(dates) == 0) {
        return(NULL)
      }

      sliderTextInput(
        inputId = ns("date_slider"),
        label = "Select Date:",
        choices = format(dates, "%Y-%m-%d"),
        selected = format(dates[1], "%Y-%m-%d"),
        grid = TRUE,
        animate = FALSE,
        width = "100%"
      )
    })

    # Handle manual date slider change
    observeEvent(input$date_slider, {
      req(available_dates())
      selected_date_str <- input$date_slider
      selected_date <- as_date(selected_date_str)

      dates <- available_dates()
      new_frame <- which(format(dates, "%Y-%m-%d") == selected_date_str)

      if (length(new_frame) > 0 && new_frame > 0) {
        updateMapWithFrame(new_frame)
      }
    })

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

    # Cache bird data to prevent repeated API calls
    cached_get_bird_data <- memoise(function(date, species) {
      scientific <- bird_spp_info |>
        filter(common_name == species) |>
        pull(scientific_name)

      if (length(scientific) == 0) {
        return(NULL)
      }

      # Format date for URL
      formatted_date <- format(date, "%Y-%m-%d")

      # Build URL for the tif file
      url_tif <- paste0(
        "https://2007581-webportal.a3s.fi/daily/",
        formatted_date, "/",
        scientific,
        "_occurrences.tif"
      )

      tryCatch(
        {
          resp <- request(url_tif) |> req_perform()
          status <- resp$status

          if (status != 200) {
            return(NULL)
          }

          tmp_file <- tempfile(fileext = ".tif")
          download_result <- download.file(url_tif, tmp_file, mode = "wb", quiet = TRUE)

          if (download_result != 0 || !file.exists(tmp_file) || file.size(tmp_file) == 0) {
            return(NULL)
          }

          r <- terra::rast(tmp_file)
          r[r == 0] <- NA
          return(r)
        },
        error = function(e) {
          return(NULL)
        }
      )
    })

    # Clear cache at midnight to get fresh data
    observe({
      invalidateLater(calculate_seconds_until_midnight())
      forget(cached_get_bird_data)
    })

    # Function to process and update map with a specific frame
    updateMapWithFrame <- function(frame_index) {
      frames <- frames_data()

      # Check if frames data exists and frame index is valid
      if (length(frames) == 0 || frame_index > length(frames) || frame_index < 1) {
        return(NULL)
      }

      frame_data <- frames[[frame_index]]

      if (is.null(frame_data) || is.null(frame_data$raster)) {
        return(NULL)
      }

      rt <- frame_data$raster
      vals <- frame_data$values

      # Use standard color palette as requested
      pal <- colorNumeric(
        palette = "magma",
        domain = vals,
        na.color = "#00000000"
      )

      # Create info card with species info
      info_card_components <- createInfoCard()
      info_card_html <- div(
        class = "leaflet-info-card",
        style = paste(
          "background-color: rgba(255, 255, 255, 0.9);",
          "padding: 15px;",
          "border-radius: 4px;",
          "border: 1px solid rgba(0,0,0,0.1);",
          "width: 220px;",
          "box-shadow: 0 2px 5px rgba(0,0,0,0.2);"
        ),
        info_card_components
      )

      # Convert htmltools tags to HTML for leaflet
      info_card_html_str <- renderTags(info_card_html)$html

      # Update the map with the current frame
      leafletProxy(ns("rasterMap")) |>
        clearImages() |>
        clearControls() |>
        addRasterImage(
          rt,
          colors = pal,
          opacity = 0.8,
          project = FALSE
        ) |>
        addLegend(
          position = "bottomright",
          pal = pal,
          values = vals,
          title = "Observations",
          opacity = 0.8
        ) |>
        addControl(
          html = info_card_html_str,
          position = "topleft"
        ) |>
        addControl(
          html = paste(
            "<div style='background-color: rgba(255,255,255,0.8); padding: 8px; border-radius: 4px;'>",
            "<strong>Date:</strong> ", format(current_date(), "%Y-%m-%d"),
            "</div>"
          ),
          position = "topright"
        )
    }

    # Create the info card with species details
    createInfoCard <- function() {
      list(
        # Photo section
        if (!is.null(photo_url())) {
          div(
            class = "info-card-photo mb-3",
            img(
              src = photo_url(),
              alt = paste("Photo of", common_name()),
              class = "img-fluid rounded"
            )
          )
        } else {
          div(
            class = "info-card-photo mb-3 text-muted",
            em("No image available")
          )
        },

        # Bird information section
        div(
          class = "info-card-details",
          # Common name
          if (!is.null(common_name())) {
            div(
              class = "mb-2",
              strong("Common Name: "),
              span(common_name())
            )
          },

          # Scientific name with optional wiki link
          if (!is.null(scientific_name())) {
            div(
              class = "mb-2 scientific-name",
              if (!is.null(wiki_link())) {
                a(
                  href = wiki_link(),
                  target = "_blank",
                  rel = "noopener",
                  em(scientific_name()),
                  class = "text-decoration-none"
                )
              } else {
                em(scientific_name())
              }
            )
          },

          # Audio player
          if (!is.null(song_url())) {
            div(
              class = "mt-3",
              tags$audio(
                class = "w-100",
                controls = NA,
                tags$source(
                  src = song_url(),
                  type = "audio/mpeg"
                ),
                "Your browser does not support the audio element."
              )
            )
          }
        )
      )
    }

    # Process and prepare data for all dates in the range
    processAllDates <- function() {
      req(date_sequence(), input$speciesPicker)

      dates <- date_sequence()
      species <- input$speciesPicker

      print(paste("Processing data for", length(dates), "dates for species:", species))

      output$statusMsg <- renderUI({
        div(
          class = "alert alert-info",
          role = "alert",
          "Loading data for all dates... This may take a moment."
        )
      })

      # Store available dates and their data
      valid_dates <- c()
      all_frames <- list()

      for (i in seq_along(dates)) {
        date <- dates[i]
        print(paste("Processing date:", date))

        # Get data for this date
        r <- cached_get_bird_data(date, species)

        if (!is.null(r)) {
          print(paste("Found data for date:", date))
          # Process raster data
          rt <- raster::raster(r)
          vals <- na.omit(raster::values(rt))

          if (length(vals) > 0) {
            # Ensure proper projection
            crs_rt <- raster::crs(rt)
            if (!is.na(crs_rt) && !sf::st_crs(crs_rt) == sf::st_crs(3857)) {
              rt <- raster::projectRaster(rt, crs = sf::st_crs(3857))
              vals <- na.omit(raster::values(rt))
            }

            # Store this date and its data
            valid_dates <- c(valid_dates, date)
            all_frames[[length(valid_dates)]] <- list(
              raster = rt,
              values = vals
            )
            print(paste("Added frame for date:", date, "- Frame count now:", length(valid_dates)))
          } else {
            print(paste("No valid values for date:", date))
          }
        } else {
          print(paste("No data available for date:", date))
        }
      }

      print(paste("Total valid dates found:", length(valid_dates)))

      # Update available dates and frame data
      if (length(valid_dates) > 0) {
        available_dates(as_date(valid_dates))
        frames_data(all_frames)
        print("Updated available_dates and frames_data reactiveVals")
      } else {
        available_dates(NULL)
        frames_data(list())
        print("No valid dates found, setting empty values")
      }

      # Show appropriate message based on results
      if (length(valid_dates) == 0) {
        output$statusMsg <- renderUI({
          div(
            class = "alert alert-warning",
            role = "alert",
            paste("No observation data available for", species, "in the selected date range.")
          )
        })
        return(FALSE)
      } else {
        output$statusMsg <- renderUI({
          div(
            class = "alert alert-success",
            role = "alert",
            paste("Data loaded successfully. Found observations for", length(valid_dates), "dates.")
          )
        })
        return(TRUE)
      }
    }

    # Changed from reactive to eventReactive to only load data when the button is clicked
    raster_data <- eventReactive(input$loadData, {
      # Process all dates and prepare data
      success <- processAllDates()
      print(paste("Data processing complete. Success:", success))

      if (success) {
        # Show the first frame
        print("Displaying first frame")
        updateMapWithFrame(1)
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
        addProviderTiles("CartoDB.Positron") |>
        setView(lng = 25, lat = 65.5, zoom = 5)
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
  })
}

box::use(
  # HTML structure (htmltools)
  htmltools[
    a, div, em, HTML, img, p, renderTags,
    strong, tagList, tags, tagQuery, span
  ],

  # Reactive components (Shiny)
  shiny[
    NS, uiOutput, actionButton, dateRangeInput,
    moduleServer, observe, observeEvent,
    reactive, req, renderUI, eventReactive,
    invalidateLater, reactiveVal, sliderInput,
    icon
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
  shinyWidgets[pickerInput],

  # Color palettes
  viridisLite[magma],
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
      class = "row",
      # Left sidebar
      div(
        class = "col-md-4",
        div(
          class = "control-panel card-shadow",
          `aria-label` = "Control Panel",
        )
      ),
      # Main content
      div(
        class = "col-md-8",
        card(
          class = "h-100 card-shadow",
          card_header(
            class = "d-flex justify-content-between align-items-center",
            tags$h2(
              class = "card_title",
              "Bird Distribution Map"
            ),
            div(
              class = "d-flex align-items-center time-display",
              uiOutput(ns("currentDateDisplay"))
            )
          ),
          card_body(
            class = "p-0",
            div(
              `aria-label` = "Bird distribution map visualization",
              leafletOutput(
                ns("rasterMap"),
                height = 600
              )
            )
          ),
          card_footer(
            class = "p-2",
            div(
              class = "animation-controls",
              uiOutput(ns("timeControlPanel"))
            )
          )
        )
      )
    )
  )

  # Use tagQuery for dynamic modifications
  control_panel <- tagQuery(base_layout)$
    find(".control-panel")$
    append(
      # Date range picker
      div(
        class = "form-group mb-3",
        tags$label(
          `for` = ns("dateRange"),
          class = "form-label",
          "Select date range:"
        ),
        dateRangeInput(
          inputId = ns("dateRange"),
          label = NULL,
          start = today() - 14,
          end = today(),
          min = "2022-01-01",
          max = today(),
          format = "yyyy-mm-dd",
          separator = " to "
        )
      ),
      # Species picker (Shiny widget)
      div(
        class = "form-group mb-3",
        tags$label(
          `for` = ns("speciesPicker"),
          class = "form-label",
          "Bird species:"
        ),
        div(
          class = "dropdown",
          pickerInput(
            ns("speciesPicker"),
            label = NULL,
            choices = species_choices,
            selected = species_choices[1],
            multiple = FALSE,
            options = list(
              `actions-box` = FALSE,
              `live-search` = TRUE,
              `size` = 10,
              `dropupAuto` = FALSE
            )
          )
        )
      ),
      # Load Data button
      div(
        class = "form-group mb-3",
        actionButton(
          inputId = ns("loadData"),
          label = "Load Data",
          class = "btn btn-primary"
        )
      ),
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
      tags$link(rel = "stylesheet", type = "text/css", href = "view/rtbm/styles.css")
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

    # Reactive values for animation control
    animation <- reactiveVal(FALSE)
    current_frame <- reactiveVal(1)
    animation_speed <- reactiveVal(5)
    
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
    
    # Current date shown in animation
    current_date <- reactive({
      req(date_sequence())
      dates <- date_sequence()
      frame <- current_frame()
      
      # Return current date based on frame
      if (frame <= length(dates)) {
        dates[frame]
      } else {
        # Reset to first date if at end
        current_frame(1)
        dates[1]
      }
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
    
    # Time control panel in footer
    output$timeControlPanel <- renderUI({
      div(
        class = "d-flex justify-content-between align-items-center",
        div(
          class = "animation-buttons",
          if (!animation()) {
            actionButton(
              inputId = ns("play_animation"), 
              label = "Play", 
              icon = icon("play"),
              class = "btn-sm btn-primary me-2"
            )
          } else {
            actionButton(
              inputId = ns("pause_animation"), 
              label = "Pause", 
              icon = icon("pause"),
              class = "btn-sm btn-secondary me-2"
            )
          }
        ),
        div(
          class = "speed-control",
          sliderInput(
            inputId = ns("speed_slider"),
            label = "Animation Speed:",
            min = 1,
            max = 10,
            value = animation_speed(),
            width = "200px"
          )
        )
      )
    })
    
    # Handle play button
    observeEvent(input$play_animation, {
      animation(TRUE)
    })
    
    # Handle pause button
    observeEvent(input$pause_animation, {
      animation(FALSE)
    })
    
    # Update animation speed
    observeEvent(input$speed_slider, {
      animation_speed(input$speed_slider)
    })
    
    # Animation loop
    observe({
      req(animation())
      
      # Get current frame and increment it
      frame <- current_frame()
      current_frame(frame + 1)
      
      # Delay based on animation speed (faster = less delay)
      delay_ms <- 2000 / animation_speed()
      invalidateLater(delay_ms)
      
      # Request data for current date
      date <- current_date()
      if (!is.null(date)) {
        updateRasterData(date, input$speciesPicker)
      }
    })

    # Handle tab selection
    observeEvent(tab_selected(), {
      print(tab_selected())
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
      
      tryCatch({
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
      }, error = function(e) {
        return(NULL)
      })
    })
    
    # Clear cache at midnight to get fresh data
    observe({
      invalidateLater(calculate_seconds_until_midnight())
      forget(cached_get_bird_data)
    })
    
    # Update data for a specific date
    updateRasterData <- function(date, species) {
      # Clear any previous status messages when starting a new load
      if (animation()) {
        # When animating, don't show loading messages for each frame
        output$statusMsg <- renderUI(NULL)
      } else {
        output$statusMsg <- renderUI({
          div(
            class = "alert alert-info",
            role = "alert",
            "Loading data... Please wait."
          )
        })
      }
      
      # Validate date
      if (date > Sys.Date()) {
        error_msg <- paste(
          "Error: Selected date is in the future. Please select a past date."
        )
        output$statusMsg <- renderUI({
          p(class = "text-danger", error_msg)
        })
        return(NULL)
      }
      
      # Get data using cache
      r <- cached_get_bird_data(date, species)
      
      if (is.null(r)) {
        if (!animation()) {
          # Only show error message if not animating
          error_msg <- paste(
            "No observation data available for",
            species,
            "on",
            format(date, "%Y-%m-%d")
          )
          output$statusMsg <- renderUI({
            p(class = "text-danger", error_msg)
          })
        }
        # Clear the map when no data
        leafletProxy(ns("rasterMap")) |>
          clearImages() |>
          clearControls()
        return(NULL)
      }
      
      # Process data and update map
      rt <- raster::raster(r)
      vals <- na.omit(raster::values(rt))
      
      if (length(vals) == 0) {
        if (!animation()) {
          output$statusMsg <- renderUI({
            div(
              class = "alert alert-warning",
              role = "alert",
              "Raster has no valid data."
            )
          })
        }
        leafletProxy(ns("rasterMap")) |>
          clearImages() |>
          clearControls()
        return(NULL)
      }
      
      crs_rt <- raster::crs(rt)
      if (!is.na(crs_rt) && !sf::st_crs(crs_rt) == sf::st_crs(3857)) {
        rt <- raster::projectRaster(rt, crs = sf::st_crs(3857))
        vals <- na.omit(raster::values(rt))
      }
      
      # Use magma color palette
      base_pal <- colorNumeric(
        magma(100),
        domain = vals,
        na.color = "#00000000"
      )
      pal_na <- function(x) {
        col <- base_pal(x)
        col[is.na(col)] <- "#00000000"
        col
      }
      
      # Create info card components using htmltools
      info_card_components <- list(
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
      
      # Create the info card with htmltools
      info_card_html <- div(
        class = "leaflet-info-card",
        style = paste(
          "background-color: var(--bs-white);",
          "padding: var(--bs-card-spacer-y) var(--bs-card-spacer-x);",
          "border-radius: var(--bs-border-radius);",
          "border: 1px solid var(--bs-border-color);",
          "width: 220px;",
          "box-shadow: var(--bs-box-shadow-sm);"
        ),
        info_card_components
      )
      
      # Convert htmltools tags to HTML for leaflet
      info_card_html_str <- renderTags(info_card_html)$html
      
      leafletProxy(ns("rasterMap")) |>
        clearImages() |>
        clearControls() |>
        addRasterImage(rt, colors = pal_na, opacity = 0.8) |>
        addLegend(
          pal = base_pal,
          values = vals,
          title = "Number of records",
          position = "topright",
          className = "info legend",
          opacity = 0.8
        ) |>
        addControl(
          html = info_card_html_str,
          position = "bottomleft",
          className = "info-card-container"
        )
        
      # Update status message to show success
      if (!animation()) {
        output$statusMsg <- renderUI({
          div(
            class = "alert alert-success",
            role = "alert",
            "Data loaded successfully."
          )
        })
      }
    }

    # Changed from reactive to eventReactive to only load data when the button is clicked
    raster_data <- eventReactive(input$loadData, {
      # Stop animation if running
      animation(FALSE)
      
      # Reset to first frame
      current_frame(1)
      
      # Get the first date in the range
      req(date_sequence())
      first_date <- date_sequence()[1]
      
      # Load data for the first date
      updateRasterData(first_date, input$speciesPicker)
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
        "Select a date range and species, then click 'Load Data' to view the distribution map. Use the play controls to animate over time."
      )
    })

    # Observe button click to trigger initial data load
    observeEvent(input$loadData, {
      raster_data()
    })
  })
}

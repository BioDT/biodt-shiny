box::use(
  # HTML structure (htmltools)
  htmltools[
    a, div, em, HTML, img, p, renderTags,
    strong, tagList, tags, tagQuery, span
  ],

  # Reactive components (Shiny)
  shiny[
    NS, dateInput, uiOutput, actionButton,
    moduleServer, observe, observeEvent,
    reactive, req, renderUI, eventReactive
  ],

  # Bootstrap components
  bslib[card, card_body, card_footer, card_header],

  # Leaflet map components
  leaflet[
    leaflet, leafletOutput, leafletProxy, renderLeaflet,
    addProviderTiles, addTiles, setView, addControl,
    addLegend, addRasterImage, clearControls,
    clearImages, colorNumeric
  ],

  # Data manipulation
  dplyr[arrange, filter, mutate, pull, select, slice],
  stringr[str_detect],
  sf[st_crs],
  tibble[as_tibble],
  jsonlite[fromJSON],
  tidyjson[spread_all],
  lubridate[today],
  httr2[request, req_perform],

  # File and data handling
  terra[rast],
  utils[download.file],
  stats[na.omit],
  raster[crs, projectRaster, raster, values, projection],
  tools[file_ext],
  grDevices[colorRampPalette],

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
          )
        )
      )
    )
  )

  # Use tagQuery for dynamic modifications
  control_panel <- tagQuery(base_layout)$
    find(".control-panel")$
    append(
      # Date picker (Shiny input)
      div(
        class = "form-group mb-3",
        tags$label(
          `for` = ns("selectedDate"),
          class = "form-label",
          "Select date:"
        ),
        dateInput(
          ns("selectedDate"),
          label = NULL,
          value = today(),
          min = as.Date("2024-11-27"),
          max = today(),
          format = "yyyy-mm-dd"
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
          ns("loadData"),
          "Load Data",
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

    # Changed from reactive to eventReactive to only load data when the button is clicked
    raster_data <- eventReactive(input$loadData, {
      # Clear any previous status messages when starting a new load
      output$statusMsg <- renderUI(NULL)
      
      # Debug prints for reactive dependencies
      print("raster_data reactive triggered")
      print(paste("selectedDate:", input$selectedDate))
      print(paste("finnish_name:", finnish_name()))
      print(paste("scientific_name:", scientific_name()))
      req(input$selectedDate, finnish_name())
      selected_date <- format(input$selectedDate, "%Y-%m-%d")

      # Print debug information
      print(paste("Selected date:", selected_date))
      print(paste("Finnish name:", finnish_name()))

      # Validate date is not in the future
      if (input$selectedDate > Sys.Date()) {
        error_msg <- paste(
          "Error: Selected date is in the future. Please select a past date."
        )
        print(error_msg)
        output$statusMsg <- renderUI({
          p(class = "text-danger", error_msg)
        })
        return(NULL)
      }

      # Show loading message
      output$statusMsg <- renderUI({
        div(
          class = "alert alert-info",
          role = "alert",
          "Loading data... Please wait."
        )
      })

      # Use the Finnish name to build the .tif URL
      url_tif <- paste0(
        "https://2007581-webportal.a3s.fi/daily/",
        selected_date, "/",
        scientific_name(),
        "_occurrences.tif"
      )
      print(paste("Attempting to access URL:", url_tif))

      # First check if the URL is accessible
      tryCatch(
        {
          resp <- request(url_tif) |> req_perform()
          status <- resp$status
          if (status != 200) {
            error_msg <- paste(
              "No observation data available for",
              common_name(),
              "on",
              selected_date
            )
            print(error_msg)
            output$statusMsg <- renderUI({
              p(class = "text-danger", error_msg)
            })
            return(NULL)
          }
        },
        error = function(e) {
          error_msg <- paste(
            "No observation data available for",
            common_name(),
            "on",
            selected_date
          )
          print(error_msg)
          output$statusMsg <- renderUI({
            p(class = "text-danger", error_msg)
          })
          return(NULL)
        }
      )

      tmp_file <- tempfile(fileext = ".tif")
      old_timeout <- getOption("timeout")
      options(timeout = 300)
      on.exit(options(timeout = old_timeout))

      tryCatch(
        {
          # Try to download the file
          download_result <- download.file(url_tif, tmp_file, mode = "wb", quiet = TRUE)
          if (download_result != 0) {
            error_msg <- paste("Failed to download file from", url_tif)
            print(error_msg)
            output$statusMsg <- renderUI({
              p(class = "text-danger", error_msg)
            })
            return(NULL)
          }

          # Check if file exists and has content
          if (!file.exists(tmp_file) || file.size(tmp_file) == 0) {
            error_msg <- "Downloaded file is empty or missing"
            print(error_msg)
            output$statusMsg <- renderUI({
              p(class = "text-danger", error_msg)
            })
            return(NULL)
          }

          r <- terra::rast(tmp_file)
          # Replace zeros with NAs
          r[r == 0] <- NA
          r
        },
        error = function(e) {
          error_msg <- paste("Error processing data:", conditionMessage(e))
          print(error_msg)
          output$statusMsg <- renderUI({
            p(class = "text-danger", error_msg)
          })
          NULL
        }
      )
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
        "Select a date and species, then click 'Load Data' to view the distribution map."
      )
    })

    # Observe changes in raster and update the map
    observe({
      # This will only trigger when raster_data() has a value, which happens after the button is clicked
      r <- raster_data()

      if (is.null(r)) {
        # Status message is now handled in the raster_data eventReactive
        leafletProxy(ns("rasterMap")) |>
          clearImages() |>
          clearControls()
        return()
      }

      rt <- raster::raster(r)
      vals <- na.omit(raster::values(rt))

      if (length(vals) == 0) {
        output$statusMsg <- renderUI({
          div(
            class = "alert alert-warning",
            role = "alert",
            "Raster has no valid data."
          )
        })
        leafletProxy(ns("rasterMap")) |>
          clearImages() |>
          clearControls()
        return()
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
      output$statusMsg <- renderUI({
        div(
          class = "alert alert-success",
          role = "alert",
          "Data loaded successfully."
        )
      })
    })
  })
}

box::use(
  # HTML structure (htmltools)
  htmltools[
    a, div, em, HTML, img, p, renderTags,
    strong, tagList, tags, tagQuery
  ],

  # Reactive components (Shiny)
  shiny[
    NS, dateInput, uiOutput, leafletOutput,
    moduleServer, observe, observeEvent,
    reactive, req, renderUI
  ],

  # Bootstrap components
  bslib[card, card_body, card_footer, card_header],

  # Leaflet map components
  leaflet[
    leaflet, leafletProxy, renderLeaflet,
    addProviderTiles, addTiles, setView, addControl,
  ],
  leaflet[
    addLegend, addRasterImage, clearControls,
    clearImages, colorNumeric,
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
    class = "container-fluid p-3",
    # Left sidebar
    div(
      class = "col-md-4 col-12",
      div(
        class = "card control-panel",
        `aria-label` = "Control Panel",
        style = "padding: var(--bs-card-spacer-y) var(--bs-card-spacer-x);"
      )
    ),
    # Main content
    div(
      class = "col-md-8 col-12",
      div(
        class = "card",
        div(
          class = "card-header d-flex justify-content-between align-items-center",
          tags$h2(
            class = "h5 mb-0",
            "Bird Distribution Map"
          )
        ),
        div(
          class = "card-body p-0",
          leafletOutput(
            ns("rasterMap"),
            height = 600,
            `aria-label` = "Bird distribution map visualization"
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
        pickerInput(
          ns("speciesPicker"),
          label = NULL,
          choices = species_choices,
          selected = species_choices[1],
          multiple = FALSE,
          options = list(
            `actions-box` = FALSE,
            `live-search` = TRUE,
            `size` = 10
          )
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
    # Add custom CSS using tags$style
    tags$style(HTML(
      "
      /* Layout */
      .container-fluid {
        --control-panel-bg: var(--bs-light);
        --control-panel-border: var(--bs-border-color);
      }

      /* Control Panel */
      .control-panel {
        border-radius: var(--bs-border-radius);
        background-color: var(--control-panel-bg);
        border: 1px solid var(--control-panel-border);
      }

      /* Cards */
      .card {
        height: 100%;
        box-shadow: var(--bs-box-shadow-sm);
      }

      /* Form elements */
      .form-group label {
        margin-bottom: 0.5rem;
        font-weight: 500;
      }

      /* Species Picker */
      .bootstrap-select .dropdown-menu {
        max-height: 300px;
      }

      /* Alert Container */
      .alert-container {
        min-height: 48px;
      }

      /* Responsive adjustments */
      @media (max-width: 768px) {
        .control-panel {
          margin-bottom: 1rem;
        }
      }

      /* Dark mode support */
      @media (prefers-color-scheme: dark) {
        .container-fluid {
          --control-panel-bg: var(--bs-dark);
          --control-panel-border: var(--bs-border-color-translucent);
        }
      }
      "
    )),
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

    # Reactive to download raster data using the Finnish name
    raster_data <- reactive({
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

    # Observe changes in raster and update the map
    observe({
      r <- raster_data()

      if (is.null(r)) {
        output$statusMsg <- renderUI({
          p(class = "text-info", "There is no observation for this selection.")
        })
        leafletProxy(ns("rasterMap")) |>
          clearImages() |>
          clearControls()
        return()
      }
      output$statusMsg <- renderUI({
        NULL
      })

      rt <- raster::raster(r)
      vals <- na.omit(raster::values(rt))

      if (length(vals) == 0) {
        output$statusMsg <- renderUI({
          p(class = "text-warning", "Raster has no valid data.")
        })
        leafletProxy(ns("rasterMap")) |>
          clearImages() |>
          clearControls()
        return()
      }

      crs_rt <- raster::crs(rt)
      if (!is.na(crs_rt) && crs_rt@projargs != "EPSG:3857") {
        rt <- raster::projectRaster(rt, crs = raster::crs("EPSG:3857"))
        vals <- na.omit(raster::values(rt))
      }

      # Replace magma with built-in color palette
      base_pal <- colorNumeric(
        colorRampPalette(c(
          "#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59",
          "#EF6548", "#D7301F", "#B30000", "#7F0000"
        ))(100),
        domain = vals,
      )
      pal_na <- function(x) {
        col <- base_pal(x)
        col[is.na(col)] <- "#00000000"
        col
      }

      # Prepare photo content
      photo_html <- if (!is.null(photo_url())) {
        img(src = photo_url(), style = "width:200px; margin-bottom:5px;")
      } else {
        p(em("No image available"))
      }

      # Common name
      common_html <- if (!is.null(common_name())) {
        p(
          strong("Common Name:"),
          " ",
          common_name()
        )
      } else {
        NULL
      }

      # Scientific name with a Wiki hyperlink if available
      sci_html <- NULL
      if (!is.null(scientific_name())) {
        if (!is.null(wiki_link())) {
          # Link to external wiki
          sci_html <- p(
            em(
              a(
                href = wiki_link(),
                target = "_blank",
                scientific_name()
              )
            )
          )
        } else {
          # Just the scientific name (no link)
          sci_html <- p(
            em(scientific_name())
          )
        }
      }

      # Song audio
      song_html <- NULL
      if (!is.null(song_url())) {
        ext <- file_ext(song_url())
        mime_type <- ifelse(ext == "mp3", "audio/mpeg", "audio/mpeg")
        song_html <- tags$audio(
          controls = NA,
          style = "width:100%;",
          tags$source(
            src = song_url(),
            type = mime_type
          ),
          "Your browser does not support the audio element."
        )
      }

      # Create the info card with htmltools
      info_card_html <- div(
        style = "background-color:white; padding:10px; border:1px solid #ccc; width:220px;",
        photo_html,
        common_html,
        sci_html,
        song_html
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
          title = "Number of records"
        ) |>
        addControl(
          html = info_card_html_str,
          position = "bottomleft"
        )
    })
  })
}

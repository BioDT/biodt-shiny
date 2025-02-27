box::use(
  shiny[NS, tagList, tags, HTML, icon, div, wellPanel, textOutput, observe, renderText, dateInput, span, strong, moduleServer, fluidRow, radioButtons, column, uiOutput, conditionalPanel, sliderInput, downloadButton, reactive, req, observeEvent, updateSliderInput, renderUI, downloadHandler],
  bslib[layout_sidebar, sidebar, card, card_header, card_body, card_footer],
  shinyWidgets[pickerInput, switchInput],
  leaflet[leafletOutput, renderLeaflet, addProviderTiles, leaflet, addTiles, setView, addEasyButton, easyButton, JS, leafletOptions, colorNumeric, leafletProxy, clearImages, clearControls, addRasterImage, evalFormula, addControl, addLegend],
  dplyr[filter, mutate, slice, select, arrange, pull],
  stringr[str_detect],
  sf[st_crs],
  tibble[as_tibble],
  jsonlite[fromJSON],
  tidyjson[spread_all],
  lubridate[today],
  httr2[request, req_perform],
  terra[rast],
  utils[download.file],
  stats[na.omit],
  raster[crs, projectRaster, raster, values, projection],
  tools[file_ext],
  grDevices[colorRampPalette],
)

# Load species info
# We should look at proper place to put this so it is loaded when needed
bird_info_url <- "https://bird-photos.a3s.fi/bird_info.json"
bird_info <- fromJSON(bird_info_url)

bird_spp_info <- bird_info |>
  spread_all() |>
  as_tibble() |>
  select(-document.id) |> 
  arrange(common_name) |>
  mutate(scientific_name = stringr::str_replace(string = scientific_name, pattern = " ", replacement = "_"))

# Use the common name for the picker
species_choices <- bird_spp_info$common_name

#' @export
rtbm_app_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    fluidRow(
        column(
          width = 4,
          wellPanel(
            dateInput(
              inputId = ns("selectedDate"),
              label = "Select date:",
              value = as.Date(today()),
              min = as.Date("2024-11-27"),
              max = as.Date(today())
            ),
            pickerInput(
              ns("speciesPicker"),
              "Bird species:",
              choices = species_choices,
              selected = species_choices[1],
              multiple = FALSE,
              options = list(
                `actions-box` = FALSE,
                `live-search` = TRUE   # <--- Add live search
              )
            ),
            textOutput(ns("statusMsg"))
          )
        ),
        column(
          width = 8,
          card(
            full_screen = TRUE,
            card_header("Bird Distribution Map"),
            card_body(
              leafletOutput(ns("rasterMap"), height = 600)
            )
          )
        )
    )
  )
}

#' @export
rtbm_app_server <- function(id, tab_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  
    observeEvent(tab_selected(),
      print(tab_selected())
    )
    # Reactives to get each field based on the *common_name* the user selected
  
  # 1) Finnish Name (needed for .tif URL)
  finnishName <- reactive({
    req(input$speciesPicker)
    fn <- bird_spp_info |>
      filter(common_name == input$speciesPicker) |>
      pull(finnish_name)
      print(fn)
    if (length(fn) == 0) return(NULL)
    fn
  })
  
  # 2) Photo URL
  photoURL <- reactive({
    req(input$speciesPicker)
    url <- bird_spp_info |>
      filter(common_name == input$speciesPicker) |>
      pull(photo_url)
    if (length(url) == 0 || url == "") return(NULL)
    url
  })
  
  # 3) Common name (English) - directly from input
  commonName <- reactive({
    req(input$speciesPicker)
    input$speciesPicker
  })
  
  # 4) Scientific name
  scientificName <- reactive({
    req(input$speciesPicker)
    sn <- bird_spp_info |>
      filter(common_name == input$speciesPicker) |>
      pull(scientific_name)
    if (length(sn) == 0) return(NULL)
    sn
  })
  
  # 4b) Wiki link (for the hyperlink on the scientific name)
  wikiLink <- reactive({
    req(input$speciesPicker)
    wl <- bird_spp_info |>
      filter(common_name == input$speciesPicker) |>
      pull(wiki_link)
    if (length(wl) == 0 || wl == "") return(NULL)
    wl
  })
  
  # 5) Song URL
  songURL <- reactive({
    req(input$speciesPicker)
    s_url <- bird_spp_info |>
      filter(common_name == input$speciesPicker) |>
      pull(song_url)
    if (length(s_url) == 0 || s_url == "") return(NULL)
    s_url
  })
  
  # Reactive to download raster data using the Finnish name
  rasterData <- reactive({
    req(input$selectedDate, finnishName())
    selected_date <- format(input$selectedDate, "%Y-%m-%d")
    
    # Print debug information
    print(paste("Selected date:", selected_date))
    print(paste("Finnish name:", finnishName()))
    
    # Validate date is not in the future
    if (input$selectedDate > Sys.Date()) {
      error_msg <- "Error: Selected date is in the future. Please select a past date."
      print(error_msg)
      output$statusMsg <- renderText(error_msg)
      return(NULL)
    }
    
    # Use the Finnish name to build the .tif URL
    url_tif <- paste0(
      "https://2007581-webportal.a3s.fi/daily/",
      selected_date, "/",
      scientificName(),
      "_occurrences.tif"
    )
    print(paste("Attempting to access URL:", url_tif))
    
    # First check if the URL is accessible
    tryCatch({
      resp <- request(url_tif) |> req_perform()
      if (resp$status_code != 200) {
        error_msg <- paste("No observation data available for", commonName(), "on", selected_date)
        print(error_msg)
        output$statusMsg <- renderText(error_msg)
        return(NULL)
      }
    }, error = function(e) {
      error_msg <- paste("No observation data available for", commonName(), "on", selected_date)
      print(error_msg)
      output$statusMsg <- renderText(error_msg)
      return(NULL)
    })
    
    tmp_file <- tempfile(fileext = ".tif")
    old_timeout <- getOption("timeout")
    options(timeout = 300)
    on.exit(options(timeout = old_timeout))
    
    tryCatch({
      # Try to download the file
      download_result <- download.file(url_tif, tmp_file, mode = "wb", quiet = TRUE)
      if (download_result != 0) {
        error_msg <- paste("Failed to download file from", url_tif)
        print(error_msg)
        output$statusMsg <- renderText(error_msg)
        return(NULL)
      }
      
      # Check if file exists and has content
      if (!file.exists(tmp_file) || file.size(tmp_file) == 0) {
        error_msg <- "Downloaded file is empty or missing"
        print(error_msg)
        output$statusMsg <- renderText(error_msg)
        return(NULL)
      }
      
      r <- terra::rast(tmp_file)
      # Replace zeros with NAs
      r[r == 0] <- NA
      r
    }, error = function(e) {
      error_msg <- paste("Error processing data:", conditionMessage(e))
      print(error_msg)
      output$statusMsg <- renderText(error_msg)
      NULL
    })
  })
  
  # Base leaflet map
  output$rasterMap <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = 25, lat = 65.5, zoom = 5)
  })
  
  # Observe changes in raster and update the map
  observe({
    r <- rasterData()
    
    if (is.null(r)) {
      output$statusMsg <- renderText("There is no observation for this selection.")
      leafletProxy(ns("rasterMap")) |> clearImages() |> clearControls()
      return()
    }
    output$statusMsg <- renderText("")
    
    rt <- raster::raster(r)
    vals <- na.omit(raster::values(rt))
    
    if (length(vals) == 0) {
      output$statusMsg <- renderText("Raster has no valid data.")
      leafletProxy(ns("rasterMap")) |> clearImages() |> clearControls()
      return()
    }
    
    crs_rt <- raster::crs(rt)
    if (!is.na(crs_rt) && crs_rt@projargs != "EPSG:3857") {
      rt <- raster::projectRaster(rt, crs = raster::crs("EPSG:3857"))
      vals <- na.omit(raster::values(rt))
    }
    
    # Replace magma with built-in color palette
    base_pal <- colorNumeric(
      colorRampPalette(c("#FFF7EC", "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#B30000", "#7F0000"))(100),
      domain = vals,
      na.color = NA
    )
    pal_na <- function(x) {
      col <- base_pal(x)
      col[is.na(col)] <- "#00000000"
      col
    }
    
    # Prepare photo content
    photo_html <- if (!is.null(photoURL())) {
      paste0("<img src='", photoURL(), "' style='width:200px; margin-bottom:5px;'>")
    } else {
      "<p><em>No image available</em></p>"
    }
    
    # Common name
    common_html <- if (!is.null(commonName())) {
      paste0("<p><strong>Common Name:</strong> ", commonName(), "</p>")
    } else {
      ""
    }
    
    # Scientific name with a Wiki hyperlink if available
    sci_html <- ""
    if (!is.null(scientificName())) {
      if (!is.null(wikiLink())) {
        # Link to external wiki
        sci_html <- paste0(
          "<p><em><a href='", wikiLink(), 
          "' target='_blank'>", scientificName(), 
          "</a></em></p>")
      } else {
        # Just the scientific name (no link)
        sci_html <- paste0("<p><em>", scientificName(), "</em></p>")
      }
    }
    
    # Song audio
    song_html <- ""
    if (!is.null(songURL())) {
      ext <- file_ext(songURL())
      mime_type <- ifelse(ext == "mp3", "audio/mpeg", "audio/mpeg")
      song_html <- paste0(
        "<audio controls style='width:100%;'>",
        "<source src='", songURL(), "' type='", mime_type, "'>",
        "Your browser does not support the audio element.</audio>"
      )
    }
    
    info_card_html <- paste0(
      "<div style='background-color:white; padding:10px; border:1px solid #ccc; width:220px;'>",
      photo_html,
      common_html,
      sci_html,
      song_html,
      "</div>"
    )
    
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
        html = info_card_html,
        position = "bottomleft"
      )
  })
  })
}

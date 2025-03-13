box::use(
  shiny[
    moduleServer,
    NS,
    tagList,
    column,
    tags,
    fluidRow,
    icon,
    actionButton,
    observeEvent,
    radioButtons,
    p,
    textOutput,
    renderText,
    showNotification,
    reactive
  ],
  bslib[card, nav_select, card_title, card_body],
  leaflet[
    leaflet,
    leafletOutput,
    renderLeaflet,
    leafletProxy,
    colorBin,
    removeLayersControl,
    addLayersControl,
    setView,
    addTiles,
    addRasterImage,
    hideGroup,
    showGroup,
    addProviderTiles,
    providerTileOptions,
    providers,
    tileOptions,
    addLegend,
    setMaxBounds
  ],
  terra[rast, values],
  waiter[Waiter],
  config,
)

#' @export
ces_rp_ui <- function(id) {
  ns <- NS(id)

  tagList(
    card(
      title = "rec_pot_map",
      full_screen = TRUE,
      card_title("Recreation potential mapping"),
      card_body(
        radioButtons(
          ns("persona"),
          "Please select a recreation potential persona from the list below:",
          choiceNames = c(
            "Hard recreationalist - visitors who prefer high-adrenaline activities that require a high level of fitness",
            "Soft recreationalist - who prefer calmer activities that do not require a high fitness level"
          ),
          choiceValues = c("hard", "soft"),
          width = "100%",
          selected = character(0)
        ),
        leafletOutput(ns("rec_pot_map_plot"), height = 600),
        p(
          "Recreation Potential (RP), an estimate of the potential capacity of a landscapes to provide opportunities for outdoor recreation, parameterized by scoring landscape features such as water bodies, types of forest."
        )
      )
    )
  )
}

#' @export
ces_rp_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    ces_path <- file.path(config$get("data_path"), "ces")

    # Create a waiter for the map container
    w <- Waiter$new(
      color = "rgba(256,256,256,0.9)"
    )

    rec_pot_map <- reactive({
      # Show the waiter
      w$show()

      # Load the raster files
      hard_rec <- tryCatch(
        {
          rast(paste0(ces_path, "/RP_maps/rec_hard_new.tif"))
        },
        error = function(e) {
          showNotification(e$message, type = "error", closeButton = TRUE, duration = NULL)
          NULL
        }
      )

      soft_rec <- tryCatch(
        {
          rast(paste0(ces_path, "/RP_maps/rec_soft_new.tif"))
        },
        error = function(e) {
          showNotification(e$message, type = "error", closeButton = TRUE, duration = NULL)
          NULL
        }
      )

      pal <- colorBin(
        "YlGnBu",
        values(hard_rec),
        bins = c(0, 0.25, 0.3, 0.33, 0.36, 0.39, 0.45, 1),
        na.color = "transparent",
        reverse = FALSE
      )

      plot <- leaflet() |>
        addTiles(group = "Open Street Map") |>
        addProviderTiles(
          providers$Esri.WorldImagery,
          providerTileOptions(zIndex = -1000),
          group = "ESRI World Imagery"
        ) |>
        addProviderTiles(providers$OpenTopoMap, providerTileOptions(zIndex = -1000), group = "Open Topo Map") |>
        setView(lng = -3.5616, lat = 57.0492, zoom = 9) |>
        setMaxBounds(lng1 = -3.860, lat1 = 56.870, lng2 = -3.000, lat2 = 57.290) |>
        addRasterImage(
          hard_rec,
          project = FALSE,
          group = "Hard recreationalist",
          opacity = 0.75,
          colors = pal,
          options = tileOptions(zIndex = 1000)
        ) |>
        addRasterImage(
          soft_rec,
          project = FALSE,
          group = "Soft recreationalist",
          opacity = 0.75,
          colors = pal,
          options = tileOptions(zIndex = 1000)
        ) |>
        addLegend(pal = pal, values = values(hard_rec), title = "Recreation", position = "bottomright") |>
        hideGroup("Hard recreationalist") |>
        hideGroup("Soft recreationalist")

      w$hide()

      # Hide the waiter after rendering the map
      plot
    })

    output$rec_pot_map_plot <- renderLeaflet({
      rec_pot_map()
    })

    observeEvent(input$persona, {
      if (input$persona == "hard") {
        leafletProxy(ns("rec_pot_map_plot")) |>
          hideGroup("Soft recreationalist") |>
          showGroup("Hard recreationalist")
      } else {
        leafletProxy(ns("rec_pot_map_plot")) |>
          hideGroup("Hard recreationalist") |>
          showGroup("Soft recreationalist")
      }
    })
  })
}

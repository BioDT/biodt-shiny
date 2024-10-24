box::use(
  shiny[NS, moduleServer, tags, observeEvent, req],
  bslib[card, card_header, card_body],
  leaflet[leafletOutput, renderLeaflet, leafletProxy, addRasterImage, addLayersControl, layersControlOptions, tileOptions, clearControls, clearImages],
)

box::use(
  app / logic / disease_outbreaks / disease_leaflet_map[read_and_project_raster]
)

#' @export
disease_map_ui <- function(id, i18n) {
  ns <- NS(id)
  card(
    id = ns("map_wrapper"),
    class = "ms-md-3 card-shadow mt-2",
    full_screen = TRUE,
    card_header(
      tags$h2(
        class = "card_title",
        i18n$translate("Map")
      )
    ),
    card_body(
      leafletOutput(
        ns("map_output")
      ),
    ),
  )
}

#' @export
disease_map_server <- function(id, map_original, leaflet_map, new_tif_upload) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(leaflet_map(), {
      req(leaflet_map())
      output_map <- leaflet_map()
      output$map_output <- renderLeaflet(output_map)
    })

    observeEvent(new_tif_upload(), {
      new_tif_raster <- new_tif_upload() |>
        read_and_project_raster()

      req(new_tif_raster)
      leafletProxy("map_output") |>
        clearImages() |>
        clearControls() |>
        addRasterImage(
          map_original(),
          opacity = 0.5,
          project = FALSE,
          options = tileOptions(zIndex = 100),
          group = "Input layer",
          layerId = "inputLayer"
        ) |>
        addRasterImage(
          new_tif_raster,
          opacity = 0.5,
          project = FALSE,
          options = tileOptions(zIndex = 101),
          group = "Output layer",
          layerId = "outputLayer"
        ) |>
        addLayersControl(
          overlayGroups = c("Input layer", "Output layer"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })
  })
}
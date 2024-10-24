box::use(
  terra[rast, project],
  leaflet[tileOptions, leaflet, addTiles, addRasterImage, setView, addLayersControl, layersControlOptions, addControl, getMapData, addProviderTiles],
)

#' @export
read_and_project_raster <- function(map_full_path) {
  map_raster <- rast(map_full_path) |> project("epsg:3857")
}

#' @export
disease_leaflet_map_basic <- function(map_raster,
                                add_control = TRUE,
                                main_map_features = TRUE) {
  leaflet_map <- leaflet() |>
    addTiles(group = "Default layer (OpenStreetMap)") |>
    setView(
      lng = 12.3601,
      lat = 51.3402,
      zoom = 5
    ) |>
    addRasterImage(
      map_raster,
      opacity = 0.5,
      project = FALSE,
      options = tileOptions(zIndex = 100),
      group = "Input layer"
    ) |>
    addLayersControl(
      baseGroups = c(
        "Default layer (OpenStreetMap)"
      ),
      overlayGroups = c("Input layer"),
      options = layersControlOptions(collapsed = FALSE)
    )

  return(leaflet_map)
}

#' @export
disease_leaflet_with_output_layer <- function(map_output_id, input_raster, output_raster) {

    
    leafletProxy(map_output_id) |> # hint: leafletProxy, removeImage, addRasterLegend, addLegend,
      clearImages() |>
      clearControls() |>
      addRasterImage(
        # input_raster,
        opacity = 0.5,
        project = FALSE,
        options = tileOptions(zIndex = 100),
        group = "Input layer",
        layerId = "inputLayer"
      ) |>
      addRasterImage(
        output_raster,
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
}


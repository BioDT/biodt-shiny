box::use(
  terra[rast, project],
  leaflet[tileOptions, leaflet, addTiles, addRasterImage, setView, leafletProxy, removeImage, addRasterLegend, addLegend, addLayersControl],
)

#' @export
read_and_project_raster <- function(map_full_path) {
  map_raster <- rast(map_full_path) |> project("epsg:3857")
}

#' @export
disease_leaflet_map <- function(map_raster,
                                add_control = TRUE,
                                main_map_features = TRUE) {

  leaflet_map <- leaflet() |>
    addTiles(group = "Open Street Map") |>
    setView(
      lng = 11.8787,
      lat = 51.3919,
      zoom = 4
    ) |>
    addRasterImage(
      map_raster,
      opacity = 0.9,
      project = FALSE,
      options = tileOptions(zIndex = 1000),
      group = "Input layer"
    ) |>
    addLayersControl(
      overlayGroups = c("Input layer")
    )

    return(leaflet_map)
}

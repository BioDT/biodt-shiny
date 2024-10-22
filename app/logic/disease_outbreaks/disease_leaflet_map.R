box::use(
  terra[rast, project],
  leaflet[tileOptions, leaflet, addTiles, addRasterImage, setView, addLayersControl, layersControlOptions],
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
    addTiles() |>
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
      overlayGroups = c("Input layer"),
      options = layersControlOptions(collapsed = FALSE)
    )

  return(leaflet_map)
}

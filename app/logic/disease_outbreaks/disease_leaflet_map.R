box::use(
  terra[rast, project],
  leaflet[leaflet, addTiles, addRasterImage, setView]
)

#' @export
read_and_project_raster <- function(map_filename) {
  map_full_path <- paste0("app/data/disease_outbreak/", map_filename, ".tif")

  map_raster <- rast(map_full_path) |> project("epsg:3857")
}

#' @export
add_map_layer <- function(output_id, tif_map_full_path) {
  # load tif raster, hardcoded for prototype (terra)
  tif_map_projected <- tif_map_full_path |>
    read_disease_outbreak_raster() |>
      project("epsg:3857")

  addRasterImage(
      tif_map_projected,
      opacity = 0.6,
      project = FALSE,
      group = "Disease Outbreaks Layer"
  )

  return(leaflet_map)
}
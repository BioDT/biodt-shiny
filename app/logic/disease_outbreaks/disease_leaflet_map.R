box::use(
  terra[rast, project],
  leaflet[leaflet, addTiles, addRasterImage, setView, leafletProxy]
)

#' @export
read_and_project_raster <- function(map_filename) {
  map_full_path <- paste0("app/data/disease_outbreak/", map_filename, ".tif")

  map_raster <- rast(map_full_path) |> project("epsg:3857")
}

#' @export
add_map_layer <- function(map_output_id, projected_tif, opa) {
  leafletProxy(map_output_id, data = projected_tif) |>
    addRasterImage(
      projected_tif,
      opacity = opa,
      project = FALSE,
    )
}
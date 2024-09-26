box::use(
  terra[rast, project],
  leaflet[leaflet, addTiles, addRasterImage, setView]
)

#' @export
read_disease_outbreak_raster <- function(map_path) {
    map_raster <- rast(map_path)
}

#' @export
make_full_tif_map_path <- function(map_name) {
  return(paste0("app/data/disease_outbreak/", map_name, ".tif"))
}

#' @export
disease_outbreak_leaflet_map <- function(id, tif_map_full_path) {
  tif_map <- reactiveVal()
  tif_map_reprojected <- reactiveVal()

  print(tif_map_full_path())

  # load tif raster, hardcoded for prototype (terra)
  tif_map_full_path() |>
    read_disease_outbreak_raster() |>
    tif_map()

  # project to right CRS (terra)
  tif_map() |>
      project("epsg:3857") |>
      tif_map_reprojected()

  # create leaflet map itself
  leaflet_map <- leaflet() |>
    addTiles() |>
    setView(
        lng = 11.8787,
        lat = 51.3919,
        zoom = 4
    ) |>
    addRasterImage(
        tif_map_reprojected(),
        opacity = 0.6,
        project = FALSE,
        group = "Disease Outbreaks Layer"
    )

    return(leaflet_map)
}
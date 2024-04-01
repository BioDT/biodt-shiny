box::use(
  terra[rast, spatSample],
  leaflet[leaflet, addTiles, addRasterImage, addRasterLegend],
  leaflet.extras[addDrawToolbar, drawMarkerOptions],
)

#' @export
read_honeybee_tif <- function(map_path) {
  map_raster <- rast(map_path)
}

#' @export
honeybee_leaflet_map <- function(map_raster,
                                 add_control = TRUE) {
  scaled_map <- map_raster |>
    spatSample(500000,
      "regular",
      as.raster = TRUE,
      warn = FALSE
    )

  leaflet_map <- leaflet() |>
    addTiles() |>
    addRasterImage(scaled_map,
      opacity = 0.5
    ) |>
    addRasterLegend(scaled_map,
                    position = "topright")

  if (add_control) {
    leaflet_map <- leaflet_map |>
      addDrawToolbar(
        polylineOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = FALSE,
        markerOptions = drawMarkerOptions(),
        circleMarkerOptions = FALSE,
        singleFeature = TRUE
      )
  }

  return(leaflet_map)
}

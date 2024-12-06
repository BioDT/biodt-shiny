box::use(
  leaflet[addTiles, setView, addMarkers, leafletProxy],
)

#' @export
grassland_update_map <- function(map_id, map_attributes) {
  if (
    is.numeric(map_attributes$lng) ||
      is.numeric(map_attributes$lat)
  ) {
    lng <- map_attributes$lng
    lat <- map_attributes$lat
    zoom <- 9
  }

  leafletProxy(map_id) |>
    addTiles() |>
    setView(
      lng = lng,
      lat = lat,
      zoom = zoom,
    ) |>
    addMarkers(
      lng = lng,
      lat = lat,
    )
}

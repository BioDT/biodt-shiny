box::use(
  leaflet[leaflet, addTiles, setView, addMarkers, leafletProxy],
)

# TODO make it working
#' export
update <- function(map_id, map_attributes) {
  print(map_id)
  print(map_attributes)

  if (
    is.numeric(map_attributes$lng) |
    is.numeric(map_attributes$lat)
  ) {
    lng <- map_attributes$lng
    lat <- map_attributes$lat
    zoom <- map_attributes$zoom
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
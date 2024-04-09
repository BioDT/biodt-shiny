box::use(
  leaflet[leaflet, addTiles, setView, addMarkers, leafletProxy],
)

#' export
update_inputmap <- function(map_id, map_attributes) {
  if (exists(map_attributes$lng) | exists(map_attributes$lat) | exists(map_attributes$lat) | exists(map_attributes$zoom)) {
    lng <- map_attributes$lng
    lat <- map_attributes$lat
    zoom <- map_attributes$zoom
  } else {
    lng <- 11.8787
    lng <- 51.3919
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
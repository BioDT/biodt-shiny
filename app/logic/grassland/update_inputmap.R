box::use(
  shiny[printStackTrace],
  leaflet[leaflet, addTiles, setView, addMarkers, leafletProxy],
)

#' export
update_inputmap <- function(map_id, map_attributes) {
  if (
    is.numeric(map_attributes$lng) |
    is.numeric(map_attributes$lat)
  ) {
    lng <- map_attributes$lng
    lat <- map_attributes$lat
    zoom <- map_attributes$zoom
  } else {
    lng <- 11.8787
    lng <- 51.3919
    zoom <- 9
  }
  
  leafletProxy(paste0(map_id, "-map")) |>
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
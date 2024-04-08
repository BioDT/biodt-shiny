box::use(
  leaflet[leaflet, addTiles, setView, addMarkers],
)

#' export
leaflet_inputmap <- function(map_options, add_control =  FALSE) {

  leaflet_map <- leaflet::leaflet() |>
    leaflet::addTiles() |>
      leaflet::setView(
        lng = map_options$lng,
        lat = map_options$lat,
        zoom = map_options$zoom,
      ) |>
      leaflet::addMarkers(
        lng = map_options$lng,
        lat = map_options$lat,
      )

  return(leaflet_map)
}
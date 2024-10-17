box::use(
  terra[rast, project],
  leaflet[leaflet, addTiles, addRasterImage, setView, leafletProxy, removeImage, addRasterLegend, addLegend]
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
      lng = 11.8787,
      lat = 51.3919,
      zoom = 4
    ) |>
    addRasterImage(
      map_raster,
      opacity = 0.9,
      project = FALSE,
      group = "Disease layer"
    ) # |>
    # addLegend(
    #   map_raster,
    #   opacity = 0.9,
    #   position = "bottomright",
    #   group = "Diseaselayer",
    #   className = "info legend Diseaselayer"
    # )

    return(leaflet_map)
}
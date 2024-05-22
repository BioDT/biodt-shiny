box::use(
  terra[rast, spatSample, deepcopy, set.values, cells],
  leaflet[leaflet, addTiles, addRasterImage, addRasterLegend, addLayersControl, layersControlOptions, addScaleBar, addAwesomeMarkers, makeAwesomeIcon, setView],
  leaflet.extras[addDrawToolbar, drawMarkerOptions],
)

#' @export
read_honeybee_tif <- function(map_path) {
  map_raster <- rast(map_path)
}

#' @export
honeybee_leaflet_map <- function(map_raster,
                                 lookup_table = NULL,
                                 add_control = TRUE) {
  
  icon.fa <- makeAwesomeIcon(icon = 'check', markerColor = 'cadetblue', library='fa', iconColor = '#fff')
  
  scaled_map <- map_raster |>
    spatSample(2000000,
      "regular",
      as.raster = TRUE,
      warn = FALSE
    )

  bee_map <- deepcopy(scaled_map)
  set.values(scaled_map, cells(scaled_map, c(0, 24)) |> unlist(), NA)
  set.values(bee_map, cells(bee_map, setdiff(0:24, c(8, 9, 10, 14, 15, 16, 18, 19))) |> unlist(), NA)

  leaflet_map <-
    leaflet() |>
    addTiles() |>
    addRasterImage(
      scaled_map,
      opacity = 0.5,
      project = FALSE,
      group = "All layers"
    ) |>
    addRasterLegend(
      scaled_map,
      opacity = 0.5,
      position = "bottomright",
      group = "Alllayers",
      className = "info legend Alllayers"
    ) |>
    addRasterImage(
      bee_map,
      opacity = 0.9,
      project = FALSE,
      group = "Beehave layers"
    ) |>
    addRasterLegend(
      bee_map,
      opacity = 0.9,
      position = "bottomright",
      group = "Beehavelayers",
      className = "info legend Beehavelayers"
    ) |>
    addLayersControl(
      c("Beehave layers", "All layers"),
      position = "topright",
      options = layersControlOptions(collapsed = FALSE)
    ) |>
    # addAwesomeMarkers(11.8787,
    #                   51.3919,
    #                   label = htmltools::HTML("<strong>Example</strong>"),
    #                   icon = icon.fa) |>
    setView(11.8787,
            51.3919,
            zoom = 5)


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
      ) |>
      addScaleBar()
  }

  return(leaflet_map)
}

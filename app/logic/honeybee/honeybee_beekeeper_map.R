box::use(
  terra[rast, spatSample, deepcopy, set.values, cells],
  leaflet[leaflet, addTiles, addRasterImage, addRasterLegend, addLayersControl, layersControlOptions],
  leaflet.extras[addDrawToolbar, drawMarkerOptions],
  htmlwidgets[onRender],
)

#' @export
read_honeybee_tif <- function(map_path) {
  map_raster <- rast(map_path)
}

#' @export
honeybee_leaflet_map <- function(map_raster,
                                 add_control = TRUE) {
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
    addRasterImage(scaled_map,
                   opacity = 0.5,
                   project = FALSE,
                   group = "All layers"
    ) |>
    addRasterLegend(scaled_map,
                    opacity = 0.5,
                    position = "topright",
                    group = "Alllayers",
                    className = "info legend Alllayers"
    ) |>  
    addRasterImage(bee_map,
                   opacity = 0.9,
                   project = FALSE,
                   group = "Beehave layers"
    ) |>  
    addRasterLegend(bee_map,
                    opacity = 0.9,
                    position = "topright",
                    group = "Beehavelayers",
                    className = "info legend Beehavelayers"
    ) |>
    addLayersControl(c("All layers", "Beehave layers"),
                     position = "topleft",
                     options = layersControlOptions(collapsed = FALSE)
    ) |>
    htmlwidgets::onRender("
      function(el, x) {
         var updateLegend = function () {
            var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);
            var selectedClass = selectedGroup.replace(' ', '');
            document.querySelectorAll('.legend').forEach(a => a.hidden=true);
            document.querySelectorAll('.legend').forEach(l => {
               if (l.classList.contains(selectedClass)) l.hidden=false;
            });
         };
         updateLegend();
         this.on('baselayerchange', el => updateLegend());
      }"
    )


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

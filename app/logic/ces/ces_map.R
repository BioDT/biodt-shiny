box::use(
  leaflet[leaflet, leafletOptions, addTiles, addProviderTiles, setView, addLegend, hideGroup, labelFormat, addRasterImage, tileOptions, providers, providerTileOptions, addControl],
  leaflet.extras[addGroupedLayersControl, groupedLayersControlOptions, addControlGPS, gpsOptions],
  terra,
  htmlwidgets[onRender],
)

#' @export
disease_leaflet_map <- function(
    recre_palette,
    biodiversity_palette,
    rec_opacity,
    key_files
) {
  leaflet_map <- leaflet(
      options = leafletOptions(
      scrollWheelZoom = TRUE,
      dragging = TRUE,
      touchZoom = TRUE,
      doubleClickZoom = TRUE,
      closePopupOnClick = FALSE,
      bounceAtZoomLimits = FALSE
    )) |>
    addTiles(group = "Open Street Map") |>
    addProviderTiles(providers$Esri.WorldImagery, providerTileOptions(zIndex = -1000), group = "ESRI World Imagery") |>
    addProviderTiles(providers$OpenTopoMap, providerTileOptions(zIndex = -1000), group = "Open Topo Map") |>
    # addProviderTiles(providers$Stadia.StamenTonerLite, providerTileOptions(zIndex = -1000), group = "Greyscale") |>
    setView(lng = -3.5616, lat = 57.0492, zoom = 9) |>
    addControlGPS(
      options = gpsOptions(
        position = "topleft",
        activate = TRUE,
        autoCenter = TRUE,
        setView = TRUE)) |>
    addRasterImage(key_files()$hard_rec, group = "Hard", project = FALSE, colors = recre_palette(), options = tileOptions(zIndex = 1000), opacity = rec_opacity()) |>
    hideGroup("Hard") |>
    addRasterImage(key_files()$soft_rec, group = "Soft", project = FALSE, colors = recre_palette(), options = tileOptions(zIndex = 1000), opacity = rec_opacity()) |>
    addLegend(
      pal = biodiversity_palette(), values = c(0, 1), title = "Biodiversity", position = "bottomright",
      labFormat = labelFormat(prefix = "", suffix = "", between = " - ")
    ) |>
    addLegend(pal = recre_palette(), values = terra::values(key_files()$hard_rec), title = "Recreation", position = "bottomright") |>
    addTiles(
      urlTemplate = "https://api.gbif.org/v2/map/occurrence/density/{z}/{x}/{y}@1x.png?style=orange.marker&bin=hex",
      attribution = "GBIF",
      group = "Biodiversity data"
    ) |> 
    addGroupedLayersControl(
      position = "bottomleft",
      baseGroups = c("Open Street Map", "ESRI World Imagery", "Open Topo Map"),
      overlayGroups = list(
        "Recreationalist" = c("Nothing", "Hard", "Soft"),
        "Biodiversity" = c("Biodiversity data", "Focal species")
      ),
      options = groupedLayersControlOptions(
        collapsed = TRUE,
        exclusiveGroups = "Recreationalist",
        groupsCollapsable = FALSE
      )
    ) |>
    hideGroup("Biodiversity data") |>
    onRender(
      "
      function(el, x) {
        var map = this;
        var grayscale = false;

        function applyGrayscale(layer) {
          if (layer instanceof L.TileLayer) {
            layer.getContainer().style.filter = grayscale ? 'grayscale(100%)' : 'none';
          }
        }

        function toggleGrayscale() {
          grayscale = !grayscale;
          map.eachLayer(applyGrayscale);
        }

        map.on('layeradd', function(e) {
          applyGrayscale(e.layer);
        });

        L.Control.GrayScaleControl = L.Control.extend({
          onAdd: function(map) {
            var btn = L.DomUtil.create('button', 'btn btn-default');
            btn.innerHTML = 'Toggle Grayscale';
            btn.onclick = toggleGrayscale;
            return btn;
          }
        });

        new L.Control.GrayScaleControl({ position: 'bottomleft' }).addTo(map);
      }"
    )
}

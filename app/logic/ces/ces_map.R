box::use(
  leaflet[leaflet, leafletOptions, addTiles, addProviderTiles, setView, addLegend, hideGroup, labelFormat, addRasterImage, tileOptions, providers, providerTileOptions, addControl],
  leaflet.extras[addGroupedLayersControl, groupedLayersControlOptions, addControlGPS, gpsOptions],
  terra,
  htmlwidgets[onRender],
)

#' @export
ces_leaflet_map <- function(
    recre_palette,
    biodiversity_palette,
    rec_opacity,
    soft_rec_filt
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
    addTiles(group = "baseLayer") |>
    # addProviderTiles(providers$Stadia.StamenTonerLite, providerTileOptions(zIndex = -1000), group = "Greyscale") |>
    setView(lng = -3.5616, lat = 57.0492, zoom = 9) |>
    addControlGPS(
      options = gpsOptions(
        position = "topleft",
        activate = TRUE,
        autoCenter = TRUE,
        setView = TRUE)) |>
    addRasterImage(soft_rec_filt, group = "RP", project = FALSE, colors = recre_palette(), options = tileOptions(zIndex = 1000), opacity = 0.5) |>
    addLegend(
      pal = biodiversity_palette(), values = c(0, 1), title = "Biodiversity", position = "bottomleft",
      labFormat = labelFormat(prefix = "", suffix = "", between = " - ")
    ) |>
    addLegend(pal = recre_palette(), values = c(0,1), title = "Recreation", position = "bottomleft") |>
    # addTiles(
    #   urlTemplate = "https://api.gbif.org/v2/map/occurrence/density/{z}/{x}/{y}@1x.png?style=orange.marker&bin=hex",
    #   attribution = "GBIF",
    #   group = "Biodiversity data"
    # ) |>
    # hideGroup("Biodiversity data") |>
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
          btn = document.getElementById('grayscale-toggle')
          var icon = btn.querySelector('i');
            if (grayscale) {
                  icon.className = 'fa-solid fa-droplet';
                  console.log('click happened');
                } else {
                  icon.className = 'fa-solid fa-droplet-slash';
                   console.log('no click');
                }
        }


        map.on('layeradd', function(e) {
          applyGrayscale(e.layer);
        });

        L.Control.GrayScaleControl = L.Control.extend({
          onAdd: function(map) {
            var btn = L.DomUtil.create('button', 'btn btn-default action-button toggle-button shiny-bound-input');
            btn.title = 'Toggle Grayscale';
            btn.id = 'grayscale-toggle';

            var icon = document.createElement('i');
            icon.className = 'fa-solid fa-droplet-slash';

            btn.appendChild(icon);

            btn.onclick = toggleGrayscale;


            var container = document.querySelector('.button-container');
            container.appendChild(btn);
          }
        });

        new L.Control.GrayScaleControl().addTo(map);
      }"
    )
}

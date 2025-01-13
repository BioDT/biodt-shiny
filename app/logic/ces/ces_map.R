box::use(
  leaflet[leaflet, leafletOptions, addTiles, addProviderTiles, setView, addLegend, hideGroup, labelFormat, addRasterImage, tileOptions, providers, providerTileOptions, addControl],
  leaflet.extras[addGroupedLayersControl, groupedLayersControlOptions, addControlGPS, gpsOptions],
  terra[rast]
)

#' @export
read_recreation_tifs <- function(map_path) {
  map_raster <- rast(map_path)
}

#' @export
disease_leaflet_map <- function(
    hard_rec,
    soft_rec,
    palette,
    biodiversity_palette,
    rec_opacity,
    selector_html
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
    setView(lng = -3.5616, lat = 57.0492, zoom = 9) |>
    addRasterImage(hard_rec, group = "Hard", project = FALSE, colors = palette, options = tileOptions(zIndex = 1000), opacity = rec_opacity) |>
      hideGroup("Hard") |>
    addRasterImage(soft_rec, group = "Soft", project = FALSE, colors = palette, options = tileOptions(zIndex = 1000), opacity = rec_opacity) |>
    addControlGPS(
      options = gpsOptions(
        position = "topleft",
        activate = TRUE,
        autoCenter = TRUE,
        setView = TRUE)) |>
    addLegend(
      pal = biodiversity_palette, values = c(0, 1), title = "Biodiversity", position = "bottomright",
      labFormat = labelFormat(prefix = "", suffix = "", between = " - ")
    ) |>
    addLegend(pal = palette, values = terra::values(hard_rec), title = "Recreation", position = "bottomright") |>
    addTiles(
      urlTemplate = "https://api.gbif.org/v2/map/occurrence/density/{z}/{x}/{y}@1x.png?style=orange.marker&bin=hex",
      attribution = "GBIF",
      group = "Biodiversity data"
    ) |> 
    hideGroup("Biodiversity data") |>
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
    )
}

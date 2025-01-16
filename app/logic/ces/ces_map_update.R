box::use(
  leaflet[leaflet, leafletProxy, leafletOptions, addTiles, addProviderTiles, setView, addLegend, hideGroup, clearGroup, showGroup, labelFormat, addRasterImage, tileOptions, providers, providerTileOptions, addControl],
  # leaflet.extras[groupedLayersControlOptions, addControlGPS, gpsOptions],
)

# The function updates all various possibilities of CES recreation & biodiverstiy map
# Which leafletProxy call is run is BASED ON VARIBLE `leaflet_proxy_type`
# `leaflet_proxy_type` can take these values:
# - "clear_species" - clears species group
# - "show_species" - shows species group
# - "add_species" - adds raster with species
# - "filter_recreation" - updates map with the filtered recreation potential
# - "add_layers" - adds radio buttons with selection of different map layers
#' @export
ces_update_map <- function(
    leaflet_proxy_type,
    map_id,
    species_raster,
    biodiversity_palette,
    hard_recreationists_raster,
    soft_recreationists_raster,
    recreation_palette
  ) {
  if  (leaflet_proxy_type == "clear_species") {
    leafletProxy(map_id) |>
      clearGroup("focal_species")
  }

  if  (leaflet_proxy_type == "show_species") {
    leafletProxy(map_id) |>
      showGroup("focal_species")
  }
}

#' @export
add_species <- function(
  map_id,
  species_raster,
  biodiversity_palette
) {
  leafletProxy(map_id) |>
    clearGroup("focal_species")

  leafletProxy(map_id) |>
    addRasterImage(
      species_raster,
      group = "focal_species",
      layerId = "merged_species_raster",
      colors = biodiversity_palette(),
      options = tileOptions(zIndex = 1000),
      opacity = 0.6
    )
}

#' @export
update_recreation <- function(
  recreation_selection,
  map_id,
  soft_recreationists_raster,
  hard_recreationists_raster,
  recreation_palette
) {
  leafletProxy(map_id) |>
    clearGroup(c("RP"))

  if (recreation_selection == "Soft"){
    leafletProxy(map_id) |>
      addRasterImage(soft_recreationists_raster, group = "RP", project = FALSE, colors = recreation_palette(), options = tileOptions(zIndex = 999), opacity = 0.5)
  }

  if (recreation_selection == "Hard"){
    leafletProxy(map_id) |>
      addRasterImage(hard_recreationists_raster, group = "RP", project = FALSE, colors = recreation_palette(), options = tileOptions(zIndex = 999), opacity = 0.5)
  }
}

#' @export
update_base_layers <- function(layer_selected, map_id) {
  leafletProxy(map_id) |>
    clearGroup("baseLayer")

  if (layer_selected == "Open Street Map") {
    leafletProxy(map_id) |>
      addTiles(layerId = "osm", group = "baseLayer")
  }

  if (layer_selected == "ESRI World Imagery") {
    leafletProxy(map_id) |>
      addProviderTiles(providers$Esri.WorldImagery, group = "baseLayer")
      # addProviderTiles(providers$Esri.WorldImagery, providerTileOptions(zIndex = -1000), group = "ESRI World Imagery")
  }

  if (layer_selected == "Open Topo Map") {
    leafletProxy(map_id) |>
      addProviderTiles(providers$OpenTopoMap, group = "baseLayer")
      # addProviderTiles(providers$OpenTopoMap, providerTileOptions(zIndex = -1000), group = "Open Topo Map")
  }
}

#' @export
update_species_biodiversity <- function(diversity_species_selected, map_id) {
  if (diversity_species_selected == FALSE) {
    leafletProxy(map_id) |>
      clearGroup("biodiversity")
  }

  if (diversity_species_selected == TRUE) {
    leafletProxy(map_id) |>
      addTiles(
        urlTemplate = "https://api.gbif.org/v2/map/occurrence/density/{z}/{x}/{y}@1x.png?style=orange.marker&bin=hex",
        attribution = "GBIF",
        group = "biodiversity"
      )
  }
}

#' @export
show_focal_species <- function(
  focal_species_checkbox,
  map_id,
  merged_raster,
  biodiversity_palette
) {
  if (focal_species_checkbox == FALSE) {
    leafletProxy(map_id) |>
      clearGroup("focal_species")
  }

  if (focal_species_checkbox == TRUE) {
    leafletProxy(map_id) |>
      addRasterImage(
        merged_raster(),
        group = "focal_species",
        layerId = "merged_species_raster",
        colors = biodiversity_palette(),
        options = tileOptions(zIndex = 1000),
        opacity = 1
      )
    
    leafletProxy(map_id) |> showGroup("focal_species")
  }
}
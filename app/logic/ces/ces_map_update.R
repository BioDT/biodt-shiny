box::use(
  leaflet[leaflet, leafletProxy, leafletOptions, addTiles, addProviderTiles, setView, addLegend, hideGroup, clearGroup, showGroup, labelFormat, addRasterImage, tileOptions, providers, providerTileOptions, addControl],
  leaflet.extras[addGroupedLayersControl, groupedLayersControlOptions, addControlGPS, gpsOptions],
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
      clearGroup("Focal species")
  }

  if  (leaflet_proxy_type == "show_species") {
    leafletProxy(map_id) |>
      showGroup("Focal species")
  }

  if  (leaflet_proxy_type == "add_species") {
    leafletProxy(map_id) |>
      addRasterImage(
        species_raster,
        group = "Focal species",
        layerId = "merged_species_raster",
        colors = biodiversity_palette(),
        options = tileOptions(zIndex = 1000),
        opacity = 0.6
      )
  }

  # if  (leaflet_proxy_type == "filter_recreation") {
  #   leafletProxy(map_id) |>
  #     clearGroup(c("Hard", "Soft")) |>
  #     addRasterImage(hard_recreationists_raster, group = "Hard", project = FALSE, colors = recreation_palette(), options = tileOptions(zIndex = 999), opacity = 0.5) #|>
  #     # addRasterImage(soft_recreationists_raster, group = "Soft", project = FALSE, colors = recreation_palette(), options = tileOptions(zIndex = 999), opacity = 0.5)
  # }
}

#' @export
update_recreation <- function(
  recreation_selection,
  map_id,
  soft_recreationists_raster,
  hard_recreationists_raster,
  recreation_palette
){
  leafletProxy(map_id) |>
    clearGroup(c("RP"))

  if (recreation_selection == "Soft"){
    print("soft")
    leafletProxy(map_id) |>
      addRasterImage(soft_recreationists_raster, group = "RP", project = FALSE, colors = recreation_palette(), options = tileOptions(zIndex = 999), opacity = 0.5)
  }

  if (recreation_selection == "Hard"){
    print("hard")
    leafletProxy(map_id) |>
      addRasterImage(hard_recreationists_raster, group = "RP", project = FALSE, colors = recreation_palette(), options = tileOptions(zIndex = 999), opacity = 0.5)
  }
}

box::use(
  leaflet[leaflet, leafletProxy, leafletOptions, addTiles, addProviderTiles, setView, addLegend, hideGroup, clearGroup, showGroup, labelFormat, addRasterImage, tileOptions, providers, providerTileOptions, addControl],
)

#' @export
show_species <- function(map_id) {
  leafletProxy(map_id) |>
    showGroup("focal_species")
}

#' @export
clear_species <- function(map_id) {
  leafletProxy(map_id) |>
    clearGroup("focal_species")
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
  input_raster,
  # soft_recreationists_raster,
  # hard_recreationists_raster,
  recreation_palette
) {
  leafletProxy(map_id) |>
    clearGroup(c("RP"))

  # print(input_raster)
  # print(!is.na(input_raster))
  if (!is.null(input_raster)) {
  leafletProxy(map_id) |>
    addRasterImage(input_raster, group = "RP", project = FALSE, colors = recreation_palette(), options = tileOptions(zIndex = 999), opacity = 0.5)
  }
  # # if (recreation_selection == "Soft"){
  #   leafletProxy(map_id) |>
  #     addRasterImage(soft_recreationists_raster, group = "RP", project = FALSE, colors = recreation_palette(), options = tileOptions(zIndex = 999), opacity = 0.5)
  # # }

  # if (recreation_selection == "Hard"){
  #   leafletProxy(map_id) |>
  #     addRasterImage(hard_recreationists_raster, group = "RP", project = FALSE, colors = recreation_palette(), options = tileOptions(zIndex = 999), opacity = 0.5)
  # }
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
  species_added,
  map_id,
  merged_raster,
  biodiversity_palette
) {
  if (focal_species_checkbox == FALSE) {
    leafletProxy(map_id) |>
      clearGroup("focal_species")
  }

  if (focal_species_checkbox == TRUE && species_added() == TRUE) {
    leafletProxy(map_id) |>
      addRasterImage(
        merged_raster(),
        group = "focal_species",
        layerId = "merged_species_raster",
        colors = biodiversity_palette(),
        options = tileOptions(zIndex = 1000),
        opacity = 0.6
      )

    leafletProxy(map_id) |> showGroup("focal_species")
  }
}

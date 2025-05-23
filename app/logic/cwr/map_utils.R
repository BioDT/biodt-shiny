box::use(
  leaflet,
  terra,
)

#' Update leaflet map with species distribution data
#' @param map_list List of raster data for each species
#' @param species Vector of selected species names
#' @param genus Selected genus of the species
#' @param session Shiny session object
#' @param stressor Selected stress variable
#' @param stress_map Stress variable raster data
#' @param stressor_range Range of stress values
#' @export
update_leaflet_map <- function(
  map_list,
  species,
  genus,
  session,
  stressor = NULL,
  stress_map = NULL,
  stressor_range = NULL,
  subset_suitability_map = FALSE
) {
  # Use leaflet proxy to clear and update the raster layer
  leaflet$leafletProxy("map", session) |>
    leaflet$clearImages() |>
    leaflet$removeControl("stress_legend")

  # Create a list to hold the base groups for the layers control
  baseGroups <- list()

  full_species <- paste(genus, species)

  for (species_name in full_species) {
    if (subset_suitability_map) {
      map_list[[species_name]][map_list[[species_name]] == 0] <- NA

      stress_map <- terra$resample(stress_map, map_list[[species_name]])
      map_list[[species_name]] <- terra$mask(
        stress_map,
        map_list[[species_name]]
      )
    }

    leaflet$leafletProxy("map", session) |>
      leaflet$addRasterImage(
        x = map_list[[species_name]],
        colors = c("red", "green"),
        opacity = 0.6,
        project = FALSE,
        group = species_name
      )
    # Add the species to the baseGroups list
    baseGroups[[species_name]] <- species_name
  }

  # Add stressor layer
  if (!is.null(stressor) && stressor != "None" && isFALSE(subset_suitability_map)) {
    # Add stressor to the baseGroups list
    baseGroups["Stressor"] <- stressor

    leaflet$leafletProxy("map", session) |>
      leaflet$addRasterImage(
        stress_map,
        colors = leaflet$colorNumeric(
          c("blue", "green", "red"),
          c(stressor_range[1], stressor_range[2]),
          na.color = "transparent"
        ),
        opacity = 0.6,
        project = FALSE,
        group = "Stressor"
      ) |>
      leaflet$addLegend(
        pal = leaflet$colorNumeric(
          c("blue", "green", "red"),
          c(stressor_range[1], stressor_range[2]) * -1,
          na.color = "transparent",
          reverse = TRUE
        ),
        values = c(stressor_range[1], stressor_range[2]) * -1,
        opacity = 0.6,
        group = "Stressor",
        layerId = "stress_legend",
        position = "bottomleft",
        labFormat = leaflet$labelFormat(transform = function(x) x * -1)
      )
  }

  # Add layers control to toggle visibility of species
  leaflet$leafletProxy("map", session) |>
    leaflet$addLayersControl(
      overlayGroups = names(baseGroups),
      options = leaflet$layersControlOptions(collapsed = FALSE)
    )
}

#' Update stress map visualization
#' @param stress_map Stress variable raster data
#' @param stressor_range Range of stress values
#' @param session Shiny session object
# update_stress_map <- function(
#   stress_map,
#   stressor_range,
#   session
# ) {
#   map_range <- terra::minmax(stress_map)

#   leaflet$leafletProxy("map_stress", session) |>
#     leaflet$clearGroup("stress") |>
#     leaflet$removeControl("stress_legend") |>
#     leaflet$addRasterImage(
#       stress_map,
#       colors = leaflet$colorNumeric(
#         c("blue", "green", "red"),
#         c(stressor_range[1], stressor_range[2]),
#         na.color = "transparent"
#       ),
#       opacity = 0.6,
#       project = FALSE,
#       group = "stress"
#     ) |>
#     leaflet$addLegend(
#       pal = leaflet$colorNumeric(
#         c("blue", "green", "red"),
#         c(stressor_range[1], stressor_range[2]),
#         na.color = "transparent"
#       ),
#       values = c(map_range[1], map_range[2]),
#       opacity = 0.6,
#       group = "stress",
#       layerId = "stress_legend",
#       position = "bottomleft"
#     )
# }

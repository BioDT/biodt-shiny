box::use(
  terra,
  echarty[ec.init],
  stats[setNames],
)

#' Create tolerance plot for selected species and stress variable
#' @param species Vector of selected species names
#' @param genus Selected genus of the species
#' @param stress_var Selected stress variable
#' @param stress_maps List of stress variable raster data
#' @param map_list List of species distribution raster data
#' @param stressor_range Range of stress values
#' @export
create_tolerance_plot <- function(
  species,
  genus,
  stress_var,
  stress_maps,
  map_list,
  stressor_range
) {
  if (is.null(stress_var) || stress_var == "None") {
    return(NULL)
  }

  stress_data <- lapply(paste(genus, species), function(species_name) {
    suitable_pixels <- which(terra$values(map_list[[species_name]]) == 1)
    stress_values <- terra$values(stress_maps[[stress_var]])[suitable_pixels] |>
      terra$na.omit() |>
      table()

    # Create a named list with zeros for all values in the range
    range_values <- as.character(stressor_range[1]:stressor_range[2])
    filled_list <- setNames(rep(0, length(range_values)), range_values)

    # Fill in the values from stress_values where names match
    filled_list[names(stress_values)] <- stress_values

    list(
      name = species_name,
      type = "line",
      color = c("#00aa00", "#ff0000", "#0000aa")[which(
        species_name == paste(genus, species)
      )],
      symbol = "none",
      showSymbol = FALSE,
      emphasis = list(disabled = TRUE),
      data = unname(as.numeric(filled_list))
    )
  })

  tolerance_plot <- ec.init()
  tolerance_plot$x$opts <- list(
    title = list(text = "Tolerance analysis"),
    tooltip = list(trigger = "axis"),
    xAxis = list(
      type = "category",
      boundaryGap = TRUE,
      name = "Stressor value",
      nameLocation = "middle",
      nameGap = 25,
      nameTextStyle = list(fontWeight = "bolder"),
      data = stressor_range[1]:stressor_range[2]
    ),
    yAxis = list(
      type = "value",
      name = "Count",
      boundaryGap = FALSE,
      nameLocation = "middle",
      nameGap = 40,
      nameTextStyle = list(fontWeight = "bolder"),
      min = 0
    ),
    series = stress_data
  )

  return(tolerance_plot)
}

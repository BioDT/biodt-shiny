# Load the libraries
box::use(
  # ggplot2,
  dplyr,
  # tidyr,
  #terra[rast, `values<-`, values, project, flip],
  terra,
  leaflet,
  stringr,
  utils,
  shiny,
  echarty
)

#' @export
plot_bird_species <- function(scenario,
                              bird_species,
                              tick,
                              prediction_folder
) {
  prediction_file <- file.path(prediction_folder, scenario, tick, paste0(bird_species, ".tif"))
  # print(prediction_file)
  if (file.exists(prediction_file)) {
    species_rast <- terra$rast(prediction_file)
    leaflet$leafletProxy("map") |>
      leaflet$removeImage("bird_species") |>
      leaflet$addRasterImage(
        species_rast,
        opacity = 0.9,
        colors = "viridis",
        project = FALSE,
        layerId = "bird_species",
        group = "bird_species"
    )
  } else {
    leaflet$leafletProxy("map") |>
      leaflet$removeImage("bird_species")
  }
}
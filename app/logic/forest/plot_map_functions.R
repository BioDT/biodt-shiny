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
    shiny$showNotification("Warning: Bird species file does not exist!", type = "error")
  }
}

#' @export
plot_tree_species <- function(data_folder, res_file) {
  simulation_file <- file.path(data_folder, res_file)
  
  if (file.exists(simulation_file)) {
    
    raster_data <- terra$rast(
          simulation_file
        )
        # raster_data <- terra$aggregate(raster_data, fact = 2, fun = mean)
        
        ext <- terra$ext(raster_data)
        
        terra$values(raster_data) |> max(na.rm = TRUE) |> is.infinite() |> print()
        if (terra$values(raster_data) |> max(na.rm = TRUE) |> is.infinite() ) {
          shiny$showNotification("Warning: Raster contains infinite values!", type = "error")
        }
        
        pal <- leaflet$colorNumeric(
          palette = "YlOrBr",
          # domain = terra$values(raster_data),
          domain = terra$values(raster_data)[is.finite(terra$values(raster_data))],
          na.color = "transparent",
          reverse = TRUE
        )
        # raster_data <- terra$aggregate(raster_data, fact = 2, fun = mean)
        
        leaflet$leafletProxy("map") |>
          leaflet$removeImage("tree_species") |>
          leaflet$clearControls() |>
          leaflet$addRasterImage(
            raster_data,
            opacity = 0.4,
            colors = pal,
            project = FALSE,
            layerId = "tree_species",
            group = "tree_species"
          ) |>
          leaflet$addLegend(
            position = "bottomright",
            pal = leaflet$colorNumeric(
              palette = "YlOrBr",
              # domain = terra$values(raster_data),
              domain = terra$values(raster_data)[is.finite(terra$values(raster_data))],
              na.color = "transparent"
            ),
            values = terra$values(raster_data),
            opacity = 0.4
          )


  } else {
    leaflet$leafletProxy("map") |>
      leaflet$removeImage("tree_species")
    shiny$showNotification("Warning: Tree species file does not exist!", type = "error")
  }
}
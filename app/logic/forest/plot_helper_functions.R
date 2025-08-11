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
        opacity = 0.3,
        colors = "viridis",
        project = FALSE,
        layerId = "bird_species",
        group = "bird_species",
        options = leaflet$tileOptions(zIndex = 2)
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
            group = "tree_species",
            options = leaflet$tileOptions(zIndex = 1)
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

#' @export
make_x_axes <- function(time_vec) {
  lapply(0:27, function(i) {
    ax <- list(type = 'category',
               data = time_vec,
               gridIndex = i)
    if (i >= 21) {                       # bottom row needs an axis title
      ax$name           <- 'year'
      ax$nameLocation   <- 'middle'
      ax$nameGap        <- 30
      ax$nameTextStyle  <- list(fontSize = 13,
                                align    = 'center',
                                color    = 'black')
    }
    ax
  })
}

#' @export
make_y_axes <- function() {
  name_map <- c(
    'AGBiomass (g/m^2)', rep(NA, 6),
    'BGBiomass (g/m^2)', rep(NA, 6),
    'Age (years)',       rep(NA, 6),
    'Woody Debris (kgDW/m^2)', rep(NA, 6)
  )

  lapply(0:27, function(i) {
    ax <- list(type = 'value', gridIndex = i)
    if (!is.na(name_map[i + 1])) {
      ax$name           <- name_map[i + 1]
      ax$nameLocation   <- 'middle'
      ax$nameGap        <- 50
      ax$nameTextStyle  <- list(fontSize = 12,
                                align    = 'center',
                                color    = 'black')
    }
    ax
  })
}

#' @export
make_grids <- function() {
  lefts <- c('4%', '18%', '32%', '46%', '60%', '74%', '88%')
  tops  <- c('10%', '31%', '56%', '81%')
  grids <- list()
  for (t in tops) {
    for (l in lefts) {
      grids[[length(grids) + 1L]] <- list(left = l,
                                          top  = t,
                                          width  = '10%',
                                          height = '12%')
    }
  }
  grids
}
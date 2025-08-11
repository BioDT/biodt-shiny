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

#' @export
make_series <- function(df) {
  mgmt      <- c("BAU","EXT10","EXT30","GTR30","NTLR","NTSR","SA")
  climate   <- c("4.5","8.5","current")
  variables <- c("AverageB.g.m2.",
                 "AverageBelowGround.g.m2.",
                 "AverageAge",
                 "WoodyDebris.kgDW.m2.")
  climate_col <- c("4.5" = "red", "8.5" = "green", "current" = "blue")

  series <- list()
  idx <- 0L
  for (var in variables) {
    for (cl in climate) {
      for (m in mgmt) {
        series_item <- list(
          data        = df[df$Management == m & df$Climate == cl, var],
          type        = "line",
          xAxisIndex  = idx %% 27,
          yAxisIndex  = idx %% 27,
          smooth      = TRUE,
          color       = climate_col[[cl]]
        )
        # only one representative series per climate needs a legend entry
        if (m == "BAU" && var == "AverageB.g.m2.") {
          series_item$name <- cl
        }
        series[[length(series) + 1L]] <- series_item
        idx <- idx + 1L
      }
    }
  }
  return(series)
}

#' @export
get_figure <- function(
    combined_data
) {

  chart <- echarty$ec.init()
  chart$x$opts <- list(
    legend = list(
      data = c('4.5', '8.5', 'current'),
      top = '1%'
    ),
    title = list(
      list(left = '8%',
           top = '5%',
           text = 'BAU',
           textStyle = list(
             fontSize = 14
           )),
      list(left = '22%',
           top = '5%',
           text = 'EXT10',
           textStyle = list(
             fontSize = 14
           )),
      list(left = '36%',
           top = '5%',
           text = 'EXT30',
           textStyle = list(
             fontSize = 14
           )),
      list(left = '50%',
           top = '5%',
           text = 'GTR30',
           textStyle = list(
             fontSize = 14
           )),
      list(left = '64%',
           top = '5%',
           text = 'NTLR',
           textStyle = list(
             fontSize = 14
           )),
      list(left = '78%',
           top = '5%',
           text = 'NTSR',
           textStyle = list(
             fontSize = 14
           )),
      list(left = '92%',
           top = '5%',
           text = 'SA',
           textStyle = list(
             fontSize = 14
           ))
    )

  )

  time_vec <- as.character(combined_data[combined_data$Management == 'BAU' & combined_data$Climate == '4.5', 'Time'])
  chart$x$opts$xAxis <- make_x_axes(time_vec)
  chart$x$opts$yAxis <- make_y_axes()
  chart$x$opts$grid  <- make_grids()
  chart$x$opts$series <- make_series(combined_data)
  
  return(chart)
}

#' @export
get_multichart <- function(
    experiment_data_file
) 
{
  
  # get data to plot
  
  all_data <- list()
  
  file_inside_zip <- file.path(experiment_data_file, "output", "TotalCohorts.txt")
  
  # get simulation start year
  lines <- readLines(file.path(experiment_data_file, "PnET-succession.txt"))
  start_year_line <- grep("^StartYear", lines, value = TRUE)
  start_year <- as.numeric(sub(".*?(\\d+).*", "\\1", start_year_line))
  
  # Read the data if the file exists
  # if (file.exists(file_inside_zip)) {
  data <- utils$read.csv(file_inside_zip)
  
  data$Time <- data$Time + start_year
  chart <- echarty$ec.init()
  chart$x$opts <- list(
    title = list(
      list(
        left = '8%',
        top = '1%',
        text = 'Average age over time',
        textStyle = list(
          fontSize = 14
        )
      ),
      list(
        left = '30%',
        top = '1%',
        text = 'Average above-ground biomass over time',
        textStyle = list(
          fontSize = 14
        )
      ),
      list(
        left = '56%',
        top = '1%',
        text = 'Woody debris over time',
        textStyle = list(
          fontSize = 14
        )
      ),
      list(
        left = '78%',
        top = '1%',
        text = 'Average below-ground biomass over time',
        textStyle = list(
          fontSize = 14
        )
      )
    ),
    grid = list(
      list(left = '4%', top = '10%', width = '20%'),
      list(left = '28%', top = '10%', width = '20%'),
      list(left = '52%', top = '10%', width = '20%'),
      list(left = '76%', top = '10%', width = '20%')
    ),
    xAxis = list(
      list(
        name = "simulation year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(data$Time),
        gridIndex = 0
        ),
      list(
        name = "simulation year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(data$Time),
        gridIndex = 1
      ),
      list(
        name = "simulation year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(data$Time),
        gridIndex = 2
      ),
      list(
        name = "simulation year",
        nameLocation = 'middle',
        nameGap = 30,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "category",
        data = as.character(data$Time),
        gridIndex = 3
      )
    ),
    yAxis = list(
      list(
        name = "year",
        nameLocation = 'middle',
        nameGap = 50,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "value",
        gridIndex = 0
        ),
      list(
        name = "g/m2",
        nameLocation = 'middle',
        nameGap = 50,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "value",
        gridIndex = 1
      ),
      list(
        name = "kgDW/m2",
        nameLocation = 'middle',
        nameGap = 50,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "value",
        gridIndex = 2
      ),
      list(
        name = "g/m2",
        nameLocation = 'middle',
        nameGap = 50,
        nameTextStyle = list(
          fontSize = 13,
          align = 'center',
          color = "black"
        ),
        type = "value",
        gridIndex = 3
      )
    ),
    tooltip = list(
      trigger = 'axis'
    ),
    # legend = list(
    #   data = c('Average age over time', 'Average above-ground biomass over time', 'Woody debris over time', 'Average below-ground biomass over time')
    # ),
    series = list(
      list(
        name = "Average age over time",
        data = data$AverageAge,
        type = "line",
        smooth = TRUE,
        xAxisIndex = 0,
        yAxisIndex = 0
      ),
      list(
        name = "Average above-ground biomass over time",
        data = data$AverageB.g.m2.,
        type = "line",
        smooth = TRUE,
        xAxisIndex = 1,
        yAxisIndex = 1
      ),
      list(
        name = "Woody debris over time",
        data = data$WoodyDebris.kgDW.m2.,
        type = "line",
        smooth = TRUE,
        xAxisIndex = 2,
        yAxisIndex = 2
      ),
      list(
        name = "Average below-ground biomass over time",
        data = data$AverageBelowGround.g.m2.,
        type = "line",
        smooth = TRUE,
        xAxisIndex = 3,
        yAxisIndex = 3
      )
      
    )
  )
  
  return(chart)
  
}
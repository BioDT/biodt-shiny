# Load the libraries
box::use(
  # Core Shiny
  shiny[
    showNotification
  ],

  # Data Handling
  utils,
  dplyr[bind_rows],
  stringr[
    str_replace,
    str_starts,
    str_extract
  ],

  # Charting
  echarty,
)

#' @export
convert_landis_output <- function(r_in) {
  ## the x and y extent of the output Landis rasters
  helper_raster <- rast(
    nrows = 1155,
    ncols = 4441,
    crs = "+proj=utm +zone=35 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
    extent = c(116000, 560100, 6604704, 6720204)
  )

  # transfer the values of the input raster into the new raster
  values(helper_raster) <- values(r_in)

  r_wgs84 <- helper_raster |>
    flip() |>
    project("EPSG:4326")

  # Replace 0 values with NA
  r_wgs84[r_wgs84 == 0] <- NA

  return(r_wgs84)
}

#' @export
get_data <- function(
  climate_scenarios,
  management_scenarios,
  years,
  data_folder
) {
  # Create an empty list to store the data
  all_data <- list()

  # Loop through each combination of climate and management scenarios
  for (climate in climate_scenarios) {
    for (management in management_scenarios) {
      # zip_file <- paste0(data_folder, "/run_landis_", paste0(climate, "_", management), "_7141504")
      file_inside_zip <- file.path(data_folder, paste0("run_landis_", paste0(climate, "_", management), "_7141504/output/", "TotalCohorts.txt"))

      # get simulation start year
      lines <- readLines(file.path(data_folder, paste0("run_landis_", paste0(climate, "_", management), "_7141504"), "PnET-succession.txt"))
      start_year_line <- grep("^StartYear", lines, value = TRUE)
      start_year <- as.numeric(sub(".*?(\\d+).*", "\\1", start_year_line))

      # Read the data if the file exists
      if (file.exists(file_inside_zip)) {
        data <- utils$read.csv(file_inside_zip)
        data$Climate <- climate
        data$Management <- management
        data$Time <- data$Time + start_year
        all_data[[paste(climate, management, sep = "_")]] <- data
      }
    }
  }
  
  # Combine all data into a single data frame
  combined_data <- bind_rows(all_data)
  return(combined_data)
}

#' Generate chart options with titles and layout
#' @export
get_figure <- function(combined_data) {
  chart <- echarty$ec.init()
  chart$x$opts <- list(
    legend = list(
      data = c('4.5', '8.5', 'current'),
      top = '1%'
    ),
    title = list(
      list(
        left = '8%',
        top = '5%',
        text = 'BAU',
        textStyle = list(fontSize = 14)
      ),
      list(
        left = '22%',
        top = '5%',
        text = 'EXT10',
        textStyle = list(fontSize = 14)
      ),
      list(
        left = '36%',
        top = '5%',
        text = 'EXT30',
        textStyle = list(fontSize = 14)
      ),
      list(
        left = '50%',
        top = '5%',
        text = 'GTR30',
        textStyle = list(fontSize = 14)
      ),
      list(
        left = '64%',
        top = '5%',
        text = 'NTLR',
        textStyle = list(fontSize = 14)
      ),
      list(
        left = '78%',
        top = '5%',
        text = 'NTSR',
        textStyle = list(fontSize = 14)
      ),
      list(
        left = '92%',
        top = '5%',
        text = 'SA',
        textStyle = list(fontSize = 14)
      )
    ),
    grid = grid_layout(),
    xAxis = generate_x_axes(combined_data),
    yAxis = generate_y_axes(),
    tooltip = list(trigger = 'axis'),
    series = generate_series_data(combined_data)
  )

  return(chart)
}

#' Generate grid layout for the chart
grid_layout <- function() {
  sapply(1:7, function(i) {
    list(
      left = sprintf('%d%%', 4 + (i-1)*14),
      top = '10%',
      width = '10%',
      height = '12%'
    )
  })
}

#' Generate X-axis configurations
generate_x_axes <- function(combined_data) {
  base_time_data <- as.character(
    combined_data[combined_data$Management == "BAU" & 
                 combined_data$Climate == "4.5", "Time"]
  )
  
  lapply(1:28, function(i) {
    list(
      name = "year",
      nameLocation = 'middle',
      nameGap = 30,
      nameTextStyle = list(
        fontSize = 13,
        align = 'center',
        color = "black"
      ),
      type = "category",
      data = base_time_data,
      gridIndex = i - 1
    )
  })
}

#' Generate Y-axis configurations
generate_y_axes <- function() {
  metric_labels <- list(
    "AGBiomass (g/m^2)" = c(0),
    "BGBiomass (g/m^2)" = c(7),
    "Age (years)" = c(14),
    "Woody Debris (kgDW/m^2)" = c(21)
  )
  
  axes <- list()
  for (i in 0:27) {
    axis <- list(type = "value", gridIndex = i)
    
    # Add labels for main axes
    for (metric in names(metric_labels)) {
      if (i %in% metric_labels[[metric]]) {
        axis$name <- metric
        axis$nameLocation <- 'middle'
        axis$nameGap <- 50
        axis$nameTextStyle <- list(
          fontSize = 12,
          align = 'center',
          color = "black"
        )
      }
    }
    
    axes[[i + 1]] <- axis
  }
  
  return(axes)
}

#' Generate series data for the chart
generate_series_data <- function(combined_data) {
  climates <- c("4.5", "8.5", "current")
  managements <- c("BAU", "EXT10", "EXT30", "GTR30", "NTLR", "NTSR", "SA")
  colors <- list("4.5" = "red", "8.5" = "green", "current" = "blue")
  metrics <- c(
    "AverageB.g.m2.", 
    "AverageBelowGround.g.m2.",
    "AverageAge",
    "WoodyDebris.kgDW.m2."
  )
  
  series_list <- list()
  for (climate in climates) {
    for (management in managements) {
      for (metric in metrics) {
        data <- combined_data[
          combined_data$Management == management & 
          combined_data$Climate == climate,
          metric
        ]
        
        series_list[[length(series_list) + 1]] <- list(
          type = "line",
          data = data,
          smooth = TRUE,
          color = colors[[climate]]
        )
      }
    }
  }
  
  return(series_list)
}

#' Get file list and parse configuration
#' @export
get_file_list <- function(input, data_folder, experiment_data) {
  if (length(experiment_data) == 0) {
    showNotification(
      "No files found matching the specified structure.",
      type = "error"
    )
    return(NULL)
  } 
  
  if (length(experiment_data) > 1) {
    showNotification(
      "Multiple files found matching the specified structure.",
      type = "error"
    )
    return(NULL)
  }

  # Get experiment path relative to data folder
  experiment <- str_replace(
    string = experiment_data,
    pattern = data_folder,
    replacement = ""
  )
  
  if (str_starts(experiment, "/")) {
    experiment <- str_replace(experiment, "/", "")
  }

  # Get file list and parse configurations
  data_file_list <- list.files(path = experiment_data, recursive = TRUE)
  config <- parse_experiment_config(experiment_data)
  
  # Get output specific configuration
  output_config <- get_output_config(
    input, 
    experiment_data,
    data_file_list, 
    experiment,
    config
  )
  
  return(c(output_config, config))
}

#' Parse experiment configuration from files
parse_experiment_config <- function(experiment_data) {
  # Read PnET configuration
  pnet_lines <- readLines(file.path(experiment_data, "PnET-succession.txt"))
  start_year <- as.numeric(
    sub(".*?(\\d+).*", "\\1", 
    grep("^StartYear", pnet_lines, value = TRUE))
  )
  timestep <- as.numeric(
    sub(".*?(\\d+).*", "\\1",
    grep("^Timestep", pnet_lines, value = TRUE))
  )
  
  # Read scenario configuration
  scenario_lines <- readLines(file.path(experiment_data, "scenario.txt"))
  duration <- as.numeric(
    sub(".*?(\\d+).*", "\\1",
    grep("^Duration", scenario_lines, value = TRUE))
  )
  
  return(list(
    start_year = start_year,
    timestep = timestep,
    duration = duration
  ))
}

#' Get output specific configuration based on input type
get_output_config <- function(input, experiment_data, data_file_list, 
                            experiment, config) {
  output_handlers <- list(
    "Above-ground biomass" = handle_biomass_output,
    "Below-ground biomass" = handle_below_ground_output,
    "Harvested biomass" = handle_harvest_output,
    "Woody Debris" = handle_woody_debris_output,
    "Max-age of selected species" = handle_max_age_output,
    "Average age of all trees" = handle_avg_age_output,
    "Median age of all trees" = handle_median_age_output
  )
  
  handler <- output_handlers[[input$output]]
  if (!is.null(handler)) {
    return(handler(
      input = input,
      experiment = experiment,
      data_file_list = data_file_list
    ))
  }
  
  return(NULL)
}

#' Handle biomass output configuration
handle_biomass_output <- function(input, experiment, data_file_list) {
  type <- case_when(
    input$species == "All species" ~ "betulaSP",
    input$species == "Birch (betulaSP)" ~ "betulaSP",
    input$species == "Pine (pinussyl)" ~ "pinussyl",
    input$species == "Spruce (piceabbies)" ~ "piceabies",
    input$species == "Other trees (other)" ~ "other"
  )
  
  res_folder <- file.path(experiment, "output/agbiomass", type)
  pattern <- paste0(
    "output/agbiomass/", type, "/AGBiomass[0-9]+\\.tif$"
  )
  
  files <- find_matching_files(data_file_list, pattern)
  
  return(list(
    res_working_folder = res_folder,
    res_file_list_tick = extract_ticks(files),
    res_folder = res_folder
  ))
}

#' Find files matching a pattern
find_matching_files <- function(data_file_list, pattern) {
  data_file_list[grep(pattern, data_file_list)]
}

#' Extract tick numbers from filenames
extract_ticks <- function(files) {
  as.integer(str_extract(files, "[0-9]+(?=[^0-9]*$)"))
}

#' Get experiment data file based on climate scenario
#' @export
get_experiment_data_file <- function(input, data_folder) {
  climate <- case_when(
    input$climate == "Current climate" ~ "current",
    input$climate == "RCP4.5" ~ "4.5",
    input$climate == "RCP8.5" ~ "8.5"
  )
  
  if (is.null(climate)) {
    return(NULL)
  }
  
  experiment_path <- file.path(
    data_folder,
    "forest",
    "simulations",
    paste0(input$management, "_", climate)
  )
  
  if (!dir.exists(experiment_path)) {
    return(NULL)
  }
  
  return(experiment_path)
}



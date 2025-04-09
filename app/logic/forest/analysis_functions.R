# Load the libraries
box::use(
  ggplot2,
  dplyr,
  tidyr,
  terra[rast, `values<-`, values, project, flip],
  stringr,
  utils,
  shiny,
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

########################################

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
      zip_file <- paste0(data_folder, "/run_landis_", paste0(climate, "_", management), "_7141504")
      file_inside_zip <- file.path(data_folder, paste0("run_landis_", paste0(climate, "_", management), "_7141504/output/", "TotalCohorts.txt"))

      # Read the data if the file exists
      if (file.exists(file_inside_zip)) {
        data <- utils$read.csv(file_inside_zip)
        data$Climate <- climate
        data$Management <- management
        all_data[[paste(climate, management, sep = "_")]] <- data
      }
    }
  }

  # Combine all data into a single data frame
  combined_data <- dplyr$bind_rows(all_data)
  return(combined_data)
}

#' @export
get_figure <- function(
  combined_data,
  column,
  unit = "",
  title = ""
) {

  plot_data <- combined_data |>
    dplyr$select(
      Time,
      all_of(column),
      Climate,
      Management
    ) |>
    tidyr$pivot_longer(
      cols = all_of(column),
      names_to = "Variable",
      values_to = "Value"
    )

  p <- ggplot2$ggplot(plot_data, ggplot2$aes(x = Time, y = Value, color = Climate)) +
    ggplot2$geom_point(position = ggplot2$position_dodge(width = 0.5), alpha = 1, size = 2) +
    ggplot2$geom_line(linewidth = 1) +
    ggplot2$facet_wrap(~Management, ncol = 7, scales = "free_x") +
    ggplot2$ylab(unit) +
    ggplot2$xlab("Time") +
    ggplot2$scale_x_continuous(breaks = seq(0, 100, 25)) +
    ggplot2$theme_minimal(base_size = 14) +
    ggplot2$labs(title = title) # +
  return(p)
}

######################

#' @export
get_file_list <- function(
  input,
  data_folder
) {
  if (input$climate == "Current climate") {
    climate <- "current"
  } else if (input$climate == "RCP4.5") {
    climate <- "4.5"
  } else if (input$climate == "RCP8.5") {
    climate <- "8.5"
  }

  # Scan for files with the specified structure
  pattern <- paste0("^.+_", climate, "_", input$management, "_.+\\$")
  experiment_data <- list.dirs(path = data_folder, full.names = TRUE, recursive = FALSE)
  experiment_data <- experiment_data[grepl(paste0("_", climate, "_", input$management, "_"), experiment_data)]
  
  experiment_data_file <- experiment_data

  if (length(experiment_data) == 0) {
    shiny$showNotification("No files found matching the specified structure.", type = "error")
  } else if (length(experiment_data) > 1) {
    shiny$showNotification("Multiple files found matching the specified structure.", type = "error")
  } else if (length(experiment_data) != 1) {
    return(NULL)
  }

  experiment <- stringr$str_replace(
    string = experiment_data,
    pattern = data_folder,
    replacement = ""
  )

  if (stringr$str_starts(experiment, "/")) {
    experiment <- stringr$str_replace(experiment, "/", "")
  }

  data_file_list <- list.files(path = experiment_data, recursive = TRUE)
  
  # get simulated years
  lines <- readLines(file.path(experiment_data, "PnET-succession.txt"))
  start_year_line <- grep("^StartYear", lines, value = TRUE)
  start_year <- as.numeric(sub(".*?(\\d+).*", "\\1", start_year_line))
  timestep_line <- grep("^Timestep", lines, value = TRUE)
  timestep <- as.numeric(sub(".*?(\\d+).*", "\\1", timestep_line))
  lines <- readLines(file.path(experiment_data, "scenario.txt"))
  duration_line <- grep("^Duration", lines, value = TRUE)
  duration <- as.numeric(sub(".*?(\\d+).*", "\\1", duration_line))
  
  if (input$output == "Above-ground biomass") {
    # todo implement update in case of "all species"
    if (input$species == "All species") {
      type <- "betulaSP"
    }
    if (input$species == "Birch (betulaSP)") {
      type <- "betulaSP"
    } else if (input$species == "Pine (pinussyl)") {
      type <- "pinussyl"
    } else if (input$species == "Spruce (piceabbies)") {
      type <- "piceabies"
    } else if (input$species == "Other trees (other)") {
      type <- "other"
    }
    res_folder <- paste0(experiment, "/output/agbiomass/", type)
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(paste0("output/agbiomass/", type, "/AGBiomass[0-9]+\\.tif$"), data_file_list)]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))
    
  } else if (input$output == "Below-ground biomass") {
    res_folder <- paste0(experiment, "/output/BelowGroundBiom/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(paste0("output/BelowGroundBiom/", "BGB[0-9]+\\.tif$"), data_file_list)]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

  } else if (input$output == "Harvested biomass") {
    res_folder <- paste0(experiment, "/output/harvest/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(
      paste0("output/harvest/", "biomass-removed-[0-9]+\\.tif$"),
      data_file_list
    )]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))
  } else if (input$output == "Woody Debris") {
    res_folder <- paste0(experiment, "/output/WoodyDebris/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(
      paste0("output/WoodyDebris/", "WoodyDebris-[0-9]+\\.tif$"),
      data_file_list
    )]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

  } else if (input$output == "Max-age of selected species") {
    if (input$species == "Birch (betulaSP)") {
      type <- "betulaSP"
    } else if (input$species == "Pine (pinussyl)") {
      type <- "pinussyl"
    } else if (input$species == "Spruce (piceabbies)") {
      type <- "piceabies"
    } else if (input$species == "All species") {
      type <- "AllSppMaxAge"
    } else if (input$species == "Other trees (other)") {
      type <- "other"
    }
    res_folder <- paste0(experiment, "/output/max-age-selected-spp/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(paste0("output/max-age-selected-spp/", type, "-[0-9]+\\.tif$"), data_file_list)]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

  } else if (input$output == "Average age of all trees") {
    res_folder <- paste0(experiment, "/output/age-all-spp/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(paste0("output/age-all-spp/", "AGE-AVG-[0-9]+\\.tif$"), data_file_list)]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

  } else if (input$output == "Median age of all trees") {
    res_folder <- paste0(experiment, "/output/age-all-spp/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(paste0("output/age-all-spp/", "AGE-MED-[0-9]+\\.tif$"), data_file_list)]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

  }

  return(list(
    experiment_data_file = experiment_data_file,
    res_working_folder = res_working_folder,
    res_file_list_tick = res_file_list_tick,
    res_folder = res_folder,
    start_year = start_year,
    timestep = timestep,
    duration = duration
  ))
}


#' @export
get_file_name <- function(input, res_folder, tick) {
  if (input$output == "Above-ground biomass") {
    res_file <- paste0(res_folder, "/AGBiomass", tick, ".tif")
  } else if (input$output == "Below-ground biomass") {
    res_file <- paste0(res_folder, "BGB", tick, ".tif")
  } else if (input$output == "Harvested biomass") {
    res_file <- paste0(res_folder, "biomass-removed-", tick, ".tif")
  } else if (input$output == "Woody Debris") {
    res_file <- paste0(res_folder, "WoodyDebris-", tick, ".tif")
  } else if (input$output == "Max-age of selected species") {
    if (input$species == "Birch (betulaSP)") {
      type <- "betulaSP"
    } else if (input$species == "Pine (pinussyl)") {
      type <- "pinussyl"
    } else if (input$species == "Spruce (piceabbies)") {
      type <- "piceabies"
    } else if (input$species == "All species") {
      type <- "AllSppMaxAge"
    } else if (input$species == "Other trees (other)") {
      type <- "other"
    }

    res_file <- paste0(res_folder, type, "-", tick, ".tif")
  } else if (input$output == "Average age of all trees") {
    res_file <- paste0(res_folder, "AGE-AVG-", tick, ".tif")
  } else if (input$output == "Median age of all trees") {
    res_file <- paste0(res_folder, "AGE-MED-", tick, ".tif")
  }

  return(res_file)
}


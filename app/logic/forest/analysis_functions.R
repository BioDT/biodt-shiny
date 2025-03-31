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
    # crs = "EPSG:32635",
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
  print("get_data")
  print(data_folder)
  # Create an empty list to store the data
  all_data <- list()

  # Loop through each combination of climate and management scenarios
  for (climate in climate_scenarios) {
    for (management in management_scenarios) {
      zip_file <- paste0(data_folder, "/run_landis_", paste0(climate, "_", management), "_7141504.zip")
      file_inside_zip <- paste0("run_landis_", paste0(climate, "_", management), "_7141504/output/", "TotalCohorts.txt")

      # Define the folder where you want to unzip the file
      temp_folder <- "temp_folder"

      # Create the folder (if it doesn't exist)
      if (!dir.exists(temp_folder)) {
        dir.create(temp_folder)
      }

      # Unzip the specific file into the "temp_folder"
      utils$unzip(zip_file, files = file_inside_zip, exdir = temp_folder)

      # Define the path to the extracted file
      extracted_file_path <- file.path(temp_folder, file_inside_zip)

      # Read the data if the file exists
      if (file.exists(extracted_file_path)) {
        data <- utils$read.csv(extracted_file_path)
        data$Climate <- climate
        data$Management <- management
        all_data[[paste(climate, management, sep = "_")]] <- data
      }

      # Remove only the extracted file (leave the temp_folder intact)
      # unlink(extracted_file_path)
    }
  }

  unlink("temp_folder", recursive = TRUE)

  # Combine all data into a single data frame
  combined_data <- dplyr$bind_rows(all_data)

  return(combined_data)
}
######################

# unit = expression(paste("AGBiomass (g/m"^2, ")"))

#' @export
get_figure <- function(
  combined_data,
  column,
  unit = "",
  title = ""
) {
  # plot_data <- combined_data |>
  #   select(Time, AverageB.g.m2., Climate, Management) |>
  #   pivot_longer(cols = AverageB.g.m2., names_to = "Variable", values_to = "Value")

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
  # ggplot2$theme(plot.title = element_text(size = 24, face = "bold"),
  #                axis.title = element_text(size = 20, face = "bold"),
  #                axis.text = element_text(size = 16),
  #                strip.text = element_text(size = 16, face = "bold"),
  #                legend.position = "bottom",
  #                legend.text = element_text(size = 14),
  #                legend.title = element_blank(),
  #                panel.background = element_rect(fill = "white"),
  #                panel.grid.major = element_line(color = "gray90"),
  #                panel.grid.minor = element_line(color = "gray95"),
  #                strip.background = element_rect(fill = "gray90", color = "gray90"),
  #                axis.text.x = element_text(angle = 45, hjust = 1)
  # )
  return(p)
}

######################

#' @export
get_file_name <- function(
  input,
  tick,
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
  pattern <- paste0("^.+_", climate, "_", input$management, "_.+\\.zip$")
  experiment_data <- list.files(path = data_folder, pattern = pattern, full.names = TRUE)
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
  experiment <- stringr$str_replace(
    string = experiment,
    pattern = ".zip",
    replacement = ""
  )
  if (stringr::str_starts(experiment, "/")) {
    experiment <- stringr::str_replace(experiment, "/", "")
  }

  data_file_list <- utils::unzip(experiment_data, exdir = tempdir(), list = TRUE)

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

    res_file_list <- data_file_list$Name[grep(
      paste0("^", res_folder, "/AGBiomass[0-9]+\\.img$"),
      data_file_list$Name
    )]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

    res_file <- paste0(res_folder, "/AGBiomass", tick, ".img")
  } else if (input$output == "Below-ground biomass") {
    res_folder <- paste0(experiment, "/output/BelowGroundBiom/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list$Name[grep(paste0("^", res_folder, "/BGB[0-9]+\\.img$"), data_file_list$Name)]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

    res_file <- paste0(res_folder, "BGB", tick, ".img")
  } else if (input$output == "Harvested biomass") {
    res_folder <- paste0(experiment, "/output/harvest/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list$Name[grep(
      paste0("^", res_folder, "/biomass-removed[0-9]+\\.img$"),
      data_file_list$Name
    )]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

    res_file <- paste0(res_folder, "biomass-removed-", tick, ".img")
  } else if (input$output == "Woody Debris") {
    res_folder <- paste0(experiment, "/output/WoodyDebris/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list$Name[grep(
      paste0("^", res_folder, "/WoodyDebris-[0-9]+\\.img$"),
      data_file_list$Name
    )]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

    res_file <- paste0(res_folder, "WoodyDebris-", tick, ".img")
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

    res_file_list <- data_file_list$Name[grep(paste0("^", res_folder, type, "-[0-9]+\\.gis$"), data_file_list$Name)]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

    res_file <- paste0(res_folder, type, "-", tick, ".gis")
  } else if (input$output == "Average age of all trees") {
    res_folder <- paste0(experiment, "/output/age-all-spp/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list$Name[grep(paste0("^", res_folder, "/AGE-AVG-[0-9]+\\.img$"), data_file_list$Name)]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

    res_file <- paste0(res_folder, "AGE-AVG-", tick, ".img")
  } else if (input$output == "Median age of all trees") {
    res_folder <- paste0(experiment, "/output/age-all-spp/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list$Name[grep(paste0("^", res_folder, "/AGE-MED-[0-9]+\\.img$"), data_file_list$Name)]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))

    res_file <- paste0(res_folder, "AGE-MED-", tick, ".img")
  }
  print("get_file_name_finished")
  return(list(
    res_file = res_file,
    experiment_data_file = experiment_data_file,
    res_working_folder = res_working_folder,
    res_file_list_tick = res_file_list_tick
  ))
}

# get_raster_image <- function(experiment_data_file, res_file){
#
#   raster_path <- rast(unzip(experiment_data_file, files = res_file, exdir = tempdir()))
#   raster_data <- convert_landis_output(raster_path)
#   raster_data <- aggregate(raster_data, fact = 2, fun = mean)
#   ext <- terra::ext(raster_data)
#   #pal <- colorNumeric(palette = "viridis", domain = values(raster_data), na.color = "transparent")
#   raster_data <- aggregate(raster_data, fact = 2, fun = mean)
#
#   return(raster_data)
#
# }

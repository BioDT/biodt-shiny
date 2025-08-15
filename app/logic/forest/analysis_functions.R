box::use(
  dplyr,
  # tidyr,
  # terra[rast, `values<-`, values, project, flip],
  stringr,
  utils,
  shiny,
  echarty,
  app /
    logic /
    forest /
    landis_io[read_landis_params],
  app /
    logic /
    forest /
    plot_helper_functions[
      make_grids,
      make_x_axes,
      make_y_axes
    ]
)


#' @export
convert_landis_output <- function(r_in, i18n) {
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

  r_wgs84
}

#' @export
get_data <- function(
    climate_scenarios,
    management_scenarios,
    years,
    data_folder,
    i18n) {
  # Create an empty list to store the data
  all_data <- list()

  # Loop through each combination of climate and management scenarios
  for (climate in climate_scenarios) {
    for (management in management_scenarios) {
      # zip_file <- paste0(data_folder, "/run_landis_", paste0(climate, "_", management), "_7141504")
      file_inside_zip <- file.path(data_folder, paste0(paste0(climate, "_", management), "/output/", "TotalCohorts.txt"))

      # get simulation parameters
      params_dir <- file.path(data_folder, paste0(paste0(climate, "_", management)))
      params <- read_landis_params(params_dir)
      start_year <- params$start_year

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
  combined_data <- dplyr$bind_rows(all_data)

  combined_data
}

#' @export
get_file_list <- function(
    inp_species,
    inp_out,
    data_folder,
    experiment_data,
    i18n) {
  if (length(experiment_data) == 0) {
    shiny$showNotification("No files found matching the specified structure.", type = "error")
    return(NULL)
  } else if (length(experiment_data) > 1) {
    shiny$showNotification("Multiple files found matching the specified structure.", type = "error")
    return(NULL)
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

  # Ensure the path is a directory we can read
  if (!dir.exists(experiment_data)) {
    shiny$showNotification(sprintf("Data directory not found: %s", experiment_data), type = "error")
    return(NULL)
  }
  data_file_list <- list.files(path = experiment_data, recursive = TRUE)

  # get simulated years via helper
  params <- read_landis_params(experiment_data)
  start_year <- params$start_year
  timestep <- params$timestep
  duration <- params$duration
  if (input$output == "Above-ground biomass") {
    # todo implement update in case of "all species"
    if (input$species == "All species") {
      type <- "betulaSP"
    }
    if (input$species == "Birch (betulaSP)") {
      type <- "betulaSP"
    } else if ("Pine (pinussyl)" %in% extract_translated_ass_array(inp_species)) {
      type <- "pinussyl"
    } else if ("Spruce (piceabbies)" %in% extract_translated_ass_array(inp_species)) {
      type <- "piceabies"
    } else if ("Other trees (other)" %in% extract_translated_ass_array(inp_species)) {
      type <- "other"
    }
    res_folder <- paste0(experiment, "/output/agbiomass/", type)
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(paste0("output/agbiomass/", type, "AGBiomass[0-9]+\\.tif$"), data_file_list)]
    res_file_list <- data_file_list[grep(
      paste0("output/agbiomass/", type, "/", "AGBiomass[0-9]+\\.tif$"),
      data_file_list
    )]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))
  } else if ("Below-ground biomass" %in% extract_translated_ass_array(inp_out)) {
    res_folder <- paste0(experiment, "/output/BelowGroundBiom/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(
      paste0("output/BelowGroundBiom/", "BGB[0-9]+\\.tif$"),
      data_file_list
    )]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))
  } else if ("Below-ground biomass" %in% extract_translated_ass_array(inp_out)) {
    res_folder <- paste0(experiment, "/output/harvest/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(
      paste0("output/harvest/", "biomass-removed-[0-9]+\\.tif$"),
      data_file_list
    )]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))
  } else if ("Below-ground biomass" %in% extract_translated_ass_array(inp_out)) {
    res_folder <- paste0(experiment, "/output/WoodyDebris/")
    res_working_folder <- res_folder
    res_file_list <- data_file_list[grep(
      paste0("output/WoodyDebris/", "WoodyDebris-[0-9]+\\.tif$"),
      data_file_list
    )]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))
  } else if ("Max-age of selected species" %in% extract_translated_ass_array(inp_species)) {
    if ("Birch (betulaSP)" %in% extract_translated_ass_array(inp_species)) {
      type <- "betulaSP"
    } else if ("Pine (pinussyl)" %in% extract_translated_ass_array(inp_species)) {
      type <- "pinussyl"
    } else if ("Spruce (piceabbies)" %in% extract_translated_ass_array(inp_species)) {
      type <- "piceabies"
    } else if ("All species" %in% extract_translated_ass_array(inp_species)) {
      type <- "AllSppMaxAge"
    } else if ("Other trees (other)" %in% extract_translated_ass_array(inp_species)) {
      type <- "other"
    }
    res_folder <- paste0(experiment, "/output/max-age-selected-spp/")
    res_working_folder <- res_folder
    res_file_list <- data_file_list[grep(
      paste0("output/max-age-selected-spp/", type, "-[0-9]+\\.tif$"),
      data_file_list
    )]

    res_file_list <- data_file_list[grep(
      paste0("output/max-age-selected-spp/", type, "-[0-9]+\\.tif$"),
      data_file_list
    )]

    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))
  } else if ("Average age of all trees)" %in% extract_translated_ass_array(inp_out)) {
    res_folder <- paste0(experiment, "/output/age-all-spp/")
    res_working_folder <- res_folder

    res_file_list <- data_file_list[grep(
      paste0("output/age-all-spp/", "AGE-AVG-[0-9]+\\.tif$"),
      data_file_list
    )]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))
  } else if ("Median age of all trees" %in% extract_translated_ass_array(inp_out)) {
    res_folder <- paste0(experiment, "/output/age-all-spp/")
    res_working_folder <- res_folder
    print("res_working_folder:::")
    print(res_working_folder)

    res_file_list <- data_file_list[grep(
      paste0("output/age-all-spp/", "AGE-MED-[0-9]+\\.tif$"),
      data_file_list
    )]
    res_file_list <- data_file_list[grep(
      paste0("output/age-all-spp/", "AGE-MED-[0-9]+\\.tif$"),
      data_file_list
    )]
    res_file_list_tick <- as.integer(stringr$str_extract(res_file_list, "[0-9]+(?=[^0-9]*$)"))
  }

  all_files_in_dir_out <- list(
    # experiment_data_file = experiment_data_file,
    res_working_folder = res_working_folder,
    res_file_list_tick = res_file_list_tick,
    res_folder = res_folder,
    start_year = start_year,
    timestep = timestep,
    duration = duration
  )

  all_files_in_dir_out
}

#' @export
get_experiment_data_file <- function(
    inp_clim,
    inp_mng,
    data_folder,
    i18n) {
  # Scan for files with the specified structure
  pattern <- paste0("^.+_", climate, "_", inp_mng, "_.+\\$")
  experiment_data <- list.dirs(path = data_folder, full.names = TRUE, recursive = FALSE)
  experiment_data <- experiment_data[grepl(paste0(climate, "_", input$management), experiment_data)]

  experiment_data_file <- experiment_data

  experiment_data_file
}


#' @export
get_file_name <- function(inp_species, inp_out, res_folder, tick, i18n) {
  if ("Above-ground biomass" %in% extract_translated_ass_array(inp_out)) {
    res_file <- paste0(res_folder, "/AGBiomass", tick, ".tif")
  } else if ("Below-ground biomass" %in% extract_translated_ass_array(inp_out)) {
    res_file <- paste0(res_folder, "BGB", tick, ".tif")
  } else if ("Below-ground biomass" %in% extract_translated_ass_array(inp_out)) {
    res_file <- paste0(res_folder, "biomass-removed-", tick, ".tif")
  } else if ("Below-ground biomass" %in% extract_translated_ass_array(inp_out)) {
    res_file <- paste0(res_folder, "WoodyDebris-", tick, ".tif")
  } else if ("Max-age of selected species" %in% extract_translated_ass_array(inp_out)) {
    if ("Birch (betulaSP)" %in% extract_translated_ass_array(inp_species)) {} else if (
      "Max-age of selected species" %in% extract_translated_ass_array(inp_out)
    ) {
      if ("Birch (betulaSP)" %in% extract_translated_ass_array(inp_species)) {
        type <- "betulaSP"
      } else if ("Pine (pinussyl)" %in% extract_translated_ass_array(inp_species)) {
        type <- "pinussyl"
      } else if ("Spruce (piceabbies)" %in% extract_translated_ass_array(inp_species)) {
        type <- "piceabies"
      } else if ("All species" %in% extract_translated_ass_array(inp_species)) {
        type <- "AllSppMaxAge"
      } else if ("Other trees (other)" %in% extract_translated_ass_array(inp_species)) {
        type <- "other"
      }

      res_file <- paste0(res_folder, type, "-", tick, ".tif")
    }
  } else if ("Average age of all trees" %in% extract_translated_ass_array(inp_out)) {
    res_file <- paste0(res_folder, "AGE-AVG-", tick, ".tif")
  } else if ("Median age of all trees" %in% extract_translated_ass_array(inp_out)) {
    all_data <- list()
  }
}

#' @export
get_bird_species_list <- function(scenario, prediction_folder) {
  prediction_folder_selected <- file.path(prediction_folder, scenario)

  bird_species_list <- list.files(path = prediction_folder_selected, recursive = TRUE, full.names = FALSE) |>
    basename() |>
    stringr$str_remove("\\.tif(\\.filepart)?$") |>
    unique() |>
    sort()

  return(bird_species_list)
}

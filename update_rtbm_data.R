#!/usr/bin/env Rscript

# Script to test RTBM data fetching and processing from remote bucket
# This script will fetch data for all available species from 2025-03-14 to today

# Load required packages
suppressPackageStartupMessages({
  library(logger)
  library(config)
  library(lubridate)
  library(fs)
  library(terra)
  library(arrow)
  library(sf)
})

# Configure logging
log_threshold(logger::DEBUG)
log_formatter(formatter_paste)

# Import RTBM data handlers modules using box::use
box::use(
  app/logic/rtbm/rtbm_data_preprocessing[
    list_available_dates,
    list_available_species,
    get_tif_url,
    match_species_name
  ],
  app/logic/rtbm/rtbm_data_handlers[
    load_bird_species_info,
    raster_to_points_df
  ]
)

# Get data paths from configuration
base_data_path <- config::get("data_path")
rtbm_data_path <- file.path(base_data_path, "rtbm")

# Initialize storage directories
dirs <- list(
  parquet_dir = file.path(rtbm_data_path, "parquet"),
  cache_dir = file.path(rtbm_data_path, "cache")
)

# Create directories if they don't exist
fs::dir_create(dirs$parquet_dir, recurse = TRUE)
fs::dir_create(dirs$cache_dir, recurse = TRUE)

# Log start of testing
log_info("Starting RTBM data fetch test...")
log_info(paste("Storage directories initialized at:", paste(unlist(dirs), collapse = ", ")))

# Load bird species information
log_info("Loading bird species information...")
bird_info <- load_bird_species_info()
if (is.null(bird_info)) {
  log_error("Failed to load bird species information")
  stop("Failed to load bird species information")
}
log_info(paste("Loaded information for", nrow(bird_info), "bird species"))

# Function to process single species data
process_species_data <- function(scientific_name, date, dirs) {
  date_str <- as.character(as.Date(date))

  # Create species directory if it doesn't exist
  species_dir <- file.path(dirs$parquet_dir, paste0("species=", scientific_name))
  fs::dir_create(species_dir, recurse = TRUE)

  # Check if we already have data for this date
  output_file <- file.path(species_dir, paste0("date=", date_str, ".parquet"))

  if (file.exists(output_file)) {
    log_info(paste("Data for", scientific_name, "on", date_str, "already exists, skipping"))
    return(TRUE)
  }

  # Get URL for the TIF file
  tif_url <- get_tif_url(date, scientific_name)

  # Create path for cache file
  tmp_file <- file.path(dirs$cache_dir, paste0(scientific_name, "_", date_str, ".tif"))

  # Download the file
  log_info(paste("Downloading data for", scientific_name, "on", date_str))
  download_result <- download.file(tif_url, tmp_file, mode = "wb", quiet = TRUE)

  if (download_result != 0 || !file.exists(tmp_file) || file.size(tmp_file) == 0) {
    log_error(paste("Failed to download data for", scientific_name, "on", date_str))
    return(FALSE)
  }

  # Process the raster file
  if (file.exists(tmp_file) && file.size(tmp_file) > 0) {
    # log_debug(paste("Processing raster file:", tmp_file))

    # # Load raster data
    r <- terra::rast(tmp_file)
    # log_debug(paste("Raster loaded with dimensions:", ncol(r), "x", nrow(r)))

    # # Print raster info for debugging
    # log_debug("Raster information:")
    # log_debug(paste("CRS:", crs(r)))
    # log_debug(paste("Extent:", paste(as.vector(ext(r)), collapse = ", ")))

    # Get raster values
    vals <- terra::values(r)
    # log_debug(paste(
    #   "Raster values summary:",
    #   "\n  Total cells:", length(vals),
    #   "\n  NA cells:", sum(is.na(vals)),
    #   "\n  Non-NA cells:", sum(!is.na(vals))
    # ))

    # Convert to points dataframe
    points_df <- raster_to_points_df(r, as.Date(date))

    if (!is.null(points_df) && nrow(points_df) > 0) {
      log_debug(paste("Created points dataframe with", nrow(points_df), "rows"))

      # Write to parquet file
      arrow::write_parquet(points_df, output_file)
      log_success(paste("Saved", nrow(points_df), "observations for", scientific_name, "on", date_str))
      return(TRUE)
    } else {
      log_warn(paste("No valid points extracted from raster for", scientific_name, "on", date_str))
      return(FALSE)
    }
  }
  return(FALSE)
}

# Set date range
start_date <- as.Date("2025-03-17")
end_date <- lubridate::today()

# List available dates in the bucket
log_info("Listing available dates in the bucket...")
available_dates <- list_available_dates()
log_info(paste("Found", length(available_dates), "date directories in the bucket"))

# Filter dates within our range
dates_in_range <- available_dates[available_dates >= start_date & available_dates <= end_date]
log_info(paste("Found", length(dates_in_range), "dates within specified range"))

# Process each date
for (date_str in dates_in_range) {
  log_info(paste("\nProcessing date:", date_str))

  # List available species for this date
  available_species <- list_available_species(date_str)
  log_info(paste("Found", nrow(available_species), "species for date", date_str))

  if (nrow(available_species) == 0) {
    log_warn(paste("No species data available for date:", date_str))
    next
  }

  # Process each species
  for (i in seq_len(nrow(available_species))) {
    scientific_name <- available_species$scientific_name[i]

    # Find matching species in bird_info
    species_idx <- match_species_name(scientific_name, bird_info)

    if (length(species_idx) == 0) {
      log_warn(paste("Species not found in bird_info:", scientific_name))
      next
    }

    species_row <- bird_info[species_idx[1], ]
    log_info(paste("Processing", species_row$common_name, "(", scientific_name, ")", "for", date_str))

    process_species_data(scientific_name, as.Date(date_str), dirs)
  }
}

log_info("\nData fetch test completed!")

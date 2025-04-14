#!/usr/bin/env Rscript

# Description:
# This script updates the Real-time Bird Monitoring (RTBM) data by:
# 1. Fetching the latest TIFF files from the source bucket.
# 2. Caching the downloaded TIFF files locally.
# 3. Converting the cached TIFFs into partitioned Parquet format.
# This script is intended to be run periodically (e.g., daily via cron)
# by the LifeWatch ERIC infrastructure.

# --- Libraries ---
# Ensure these packages are installed in the execution environment
library(httr2)
library(stringr)
library(fs)
library(purrr)
library(terra)
library(arrow)
library(lubridate)
library(jsonlite) # Added for reading bird_info.json
library(optparse) # Added for command line argument parsing
library(dplyr) # Added for bind_rows if needed, and potentially in raster_to_points_df
library(sf)       # Added for CRS projection
library(stats)    # Added for complete.cases used in raster_to_points_df
# library(logger) # Optional: for more advanced logging

# --- Standalone Configuration ---
# Define paths relative to the project root assuming the script is run from there.
# Adjust if the script's working directory is different.
PROJECT_ROOT <- getwd() # Assuming script is run from project root
RTBM_CACHE_PATH <- fs::path(PROJECT_ROOT, "app", "data", "rtbm", "cache")
RTBM_PARQUET_PATH <- fs::path(PROJECT_ROOT, "app", "data", "rtbm", "parquet")
BIRD_INFO_JSON_PATH <- fs::path(PROJECT_ROOT, "app", "data", "rtbm", "bird_info.json")

# !!! IMPORTANT: Replace this placeholder with the actual base URL for the TIFF files !!!
RTBM_TIFF_BUCKET_URL_BASE <- "https://2007581-webportal.a3s.fi/daily/"
# --- End Standalone Configuration ---

# --- Configuration & Setup ---
message("--- Script Start: RTBM Parquet Update - ", Sys.time(), " ---")

# Default start date if no specific date is provided
DEFAULT_START_DATE <- "2025-03-30"

# Create directories if they don't exist
if (!dir_exists(RTBM_CACHE_PATH)) dir_create(RTBM_CACHE_PATH, recurse = TRUE)
if (!dir_exists(RTBM_PARQUET_PATH)) dir_create(RTBM_PARQUET_PATH, recurse = TRUE)

# --- Argument Parsing --- START ---
option_list <- list(
  make_option(c("-d", "--date"),
    type = "character", default = NULL,
    help = "Target date in YYYY-MM-DD format. If omitted, processes from 2025-01-01 to yesterday.",
    metavar = "YYYY-MM-DD"
  ),
  make_option(c("-f", "--force"),
    action = "store_true", default = FALSE,
    help = "Force overwrite of existing Parquet files"
  )
)

opt_parser <- OptionParser(option_list = option_list)
opts <- parse_args(opt_parser)

# Determine target date(s)
date_sequence <- NULL
if (!is.null(opts$date)) {
  # Specific date provided
  parsed_date <- ymd(opts$date, quiet = TRUE)
  if (!is.na(parsed_date)) {
    date_sequence <- c(parsed_date)
    message(sprintf("Processing specified target date: %s", format(date_sequence[1], "%Y-%m-%d")))
  } else {
    stop(sprintf("Invalid date format provided ('%s'). Please use YYYY-MM-DD.", opts$date))
  }
} else {
  # No specific date, use range from default start to yesterday
  start_date <- ymd(DEFAULT_START_DATE, quiet = TRUE)
  end_date <- Sys.Date() - days(1)
  if (is.na(start_date) || start_date > end_date) {
    stop(sprintf(
      "Invalid date range calculated (Start: %s, End: %s). Check DEFAULT_START_DATE.",
      DEFAULT_START_DATE, format(end_date, "%Y-%m-%d")
    ))
  }
  date_sequence <- seq(start_date, end_date, by = "day")
  message(sprintf(
    "Processing date range from %s to %s (%d dates).",
    format(start_date, "%Y-%m-%d"),
    format(end_date, "%Y-%m-%d"),
    length(date_sequence)
  ))
}
# --- Argument Parsing --- END ---

# --- Function Definitions ---

#' Load Scientific Names from bird_info.json
load_scientific_names <- function() {
  if (!file_exists(BIRD_INFO_JSON_PATH)) {
    message("Error: bird_info.json not found at ", BIRD_INFO_JSON_PATH)
    return(NULL)
  }
  tryCatch(
    {
      # simplifyVector = TRUE is the default and often helpful,
      # but might turn a list of objects into a data frame.
      bird_info <- jsonlite::fromJSON(BIRD_INFO_JSON_PATH, simplifyVector = TRUE)
      sci_names <- NULL

      # PRIORITIZE getting names if it's a list (even if simplified to df)
      if (is.list(bird_info) && !is.null(names(bird_info))) {
        sci_names <- names(bird_info)
        # FALLBACK: Check for column if names weren't useful or structure is different
      } else if (is.data.frame(bird_info) && "scientific_name" %in% names(bird_info)) {
        sci_names <- bird_info$scientific_name
      } else {
        message("Error: Could not extract scientific names from bird_info.json structure. Checked list names and 'scientific_name' column.")
        return(NULL)
      }

      # Filter out any NULL/NA/empty names
      valid_names <- sci_names[!is.na(sci_names) & sci_names != ""]
      if (length(valid_names) == 0) {
        message("Error: No valid scientific names extracted from bird_info.json")
        return(NULL)
      }
      return(unique(valid_names))
    },
    error = function(e) {
      message("Error reading or parsing bird_info.json: ", e$message)
      return(NULL)
    }
  )
}

#' Fetch and Cache TIFF Files for a Specific Date (Defaults to Yesterday)
fetch_and_cache_tiffs <- function(date_str, names_to_process, force_update = FALSE) {
  # --- Debugging Start ---
  message("--- Inside fetch_and_cache_tiffs for date: ", date_str, " ---")
  message("Number of names to process: ", length(names_to_process))
  if (length(names_to_process) > 0) {
    message("First name: '", names_to_process[1], "'")
    message("Data type of names_to_process: ", typeof(names_to_process))
    message("Is it a vector? ", is.vector(names_to_process))
  } else {
    message("names_to_process is empty!")
    return(invisible(NULL))
  }
  # --- Debugging End ---

  # Define paths and URLs based on config
  # Paths are now defined globally at the top of the script
  # Now check if the constructed path is empty (e.g., if config value was "")
  # --- Add check for RTBM_CACHE_PATH ---
  if (is.null(RTBM_CACHE_PATH) || !nzchar(RTBM_CACHE_PATH)) {
    # This check might be redundant now but kept for safety
    stop("Internal error: Constructed RTBM_CACHE_PATH is invalid.")
  }
  # --- End check ---

  # URL base is now defined globally at the top of the script
  if (is.null(RTBM_TIFF_BUCKET_URL_BASE) || !nzchar(RTBM_TIFF_BUCKET_URL_BASE) || RTBM_TIFF_BUCKET_URL_BASE == "YOUR_TIFF_BUCKET_BASE_URL_HERE") {
    stop("Configuration error: RTBM_TIFF_BUCKET_URL_BASE is not set correctly in the script. Please edit the placeholder.")
  }

  # Ensure cache directory exists
  if (!dir_exists(RTBM_CACHE_PATH)) dir_create(RTBM_CACHE_PATH, recurse = TRUE)

  # --- RESTORED IMAP LOOP ---
  # Explicitly use purrr::imap
  download_results <- purrr::imap(names_to_process, \(species_name, index) {
    # --- Refined Check for species_name ---
    # Ensure species_name is a single, non-NA, non-empty character string
    if (!is.character(species_name) || length(species_name) != 1 || is.na(species_name) || !nzchar(trimws(species_name))) {
      # Log the problematic value if possible
      species_val_log <- ifelse(is.null(species_name), "NULL",
        ifelse(is.na(species_name), "NA",
          ifelse(length(species_name) == 0, "character(0)",
            paste0("'", species_name, "'")
          )
        )
      )
      message("  - Skipped (Invalid species name at index: ", index, ", value: ", species_val_log, ")")
      return(list(status = "skipped_invalid_species"))
    }
    # --- End Refined Check ---

    message("- Processing species: ", species_name)

    # 1. Construct the local filename (spaces replaced by underscores)
    species_name_underscore <- gsub(" ", "_", species_name)
    local_file_name <- paste0(species_name_underscore, "_occurrences.tif")
    target_local_path <- file.path(RTBM_CACHE_PATH, local_file_name)

    # 2. Check cache using the correct local filename
    # -- Add explicit check for file_exists NA --
    file_exists_check <- file.exists(target_local_path)
    if (is.na(file_exists_check)) {
      warning(paste("Could not determine existence for:", target_local_path, "- file.exists returned NA. Skipping cache check for this file."))
      # Assume not cached if existence is NA
      file_exists_check <- FALSE
    }
    # -- End explicit check --

    if (file_exists_check) { # Removed force_update check
      message("  - Skipped (already cached): ", local_file_name)
      return(list(status = "skipped_cached", file = local_file_name))
    }

    # 3. Construct the URL (using the same underscore-replaced name)
    file_name_for_url <- local_file_name # Use the same name
    target_url <- paste0(RTBM_TIFF_BUCKET_URL_BASE, date_str, "/", file_name_for_url)

    # Attempt download directly using utils::download.file
    tryCatch(
      {
        message("  - Attempting download: ", target_url)
        download_status <- utils::download.file(target_url, target_local_path, mode = "wb", quiet = TRUE)

        if (download_status == 0) {
          message("    - Success: Downloaded to ", target_local_path)
          return(list(status = "downloaded", file = local_file_name))
        } else {
          # download.file returns non-zero on error - treat as a download failure
          message("    - Failed (Download Error Status: ", download_status, "): ", target_url)
          if (file.exists(target_local_path)) {
            try(file.remove(target_local_path), silent = TRUE)
          }
          return(list(status = "error_download_status", url = target_url, code = download_status))
        }
      },
      error = function(e) {
        if (grepl("404 Not Found", e$message) || grepl("cannot open URL", e$message)) {
          message("    - Failed (404 Not Found): ", target_url)
          return(list(status = "skipped_404", url = target_url))
        } else {
          message("    - Failed (Other Error): ", target_url)
          message("      Error details: ", e$message)
          if (file.exists(target_local_path)) {
            try(file.remove(target_local_path), silent = TRUE)
          }
          return(list(status = "error_download", url = target_url, code = "unknown"))
        }
      }
    )
  }) # End of imap
  # --- END RESTORED IMAP LOOP ---

  # --- RESTORED SUMMARIZATION LOGIC ---
  if (is.list(download_results) && length(download_results) > 0) {
    valid_results_intermediate <- download_results[!sapply(download_results, is.null)]
    is_valid_list_with_status <- function(x) is.list(x) && "status" %in% names(x)
    valid_results <- Filter(is_valid_list_with_status, valid_results_intermediate)

    if (length(valid_results) > 0) {
      status_values <- map_chr(valid_results, "status")
      status_summary <- table(status_values)
      message("TIFF Download/Check Summary for ", date_str, ":")
      for (status_name in names(status_summary)) {
        message("- ", status_name, ": ", status_summary[[status_name]])
      }
    } else {
      message("No valid download results to summarize for ", date_str)
    }
  } else {
    message("Warning: download_results was not a valid list or was empty for ", date_str)
  }
  # --- END RESTORED SUMMARIZATION LOGIC ---

  invisible(NULL)
}

#' Convert raster to points dataframe (Internal Version)
#' @param r Raster object
#' @param date Date for the data
#' @return Data frame with x, y coordinates, value, and date, or empty DF on error
raster_to_points_df <- function(r, date = NULL) {
  # Note: Removed logger dependency for standalone script
  tryCatch(
    {
      # --- CRS Check and Projection --- 
      # Target CRS is WGS84 (degrees)
      target_crs <- "EPSG:4326"
      current_crs_info <- terra::crs(r, proj = TRUE)

      # Check if the current CRS is geographic (uses degrees)
      # Simple check: if it's not geographic, assume it needs projection
      is_geographic <- grepl("+proj=longlat", current_crs_info, fixed = TRUE) || 
                       grepl("UNIT[\"']degree[\"']", terra::crs(r, describe=TRUE)$proj_unit, ignore.case=TRUE)

      if (!is_geographic) {
        message(sprintf("Projecting raster from %s to %s", terra::crs(r, describe=TRUE)$name, target_crs))
        # Ensure sf is available for projection
        # requireNamespace("sf", quietly = TRUE) 
        r_projected <- tryCatch({
            terra::project(r, target_crs)
          },
          error = function(e) {
            stop(paste("Error during raster projection:", e$message))
          }
        )
        # Convert the *projected* raster to data frame
        df <- terra::as.data.frame(r_projected, xy = TRUE)
      } else {
         # If already geographic, just use the original data frame
         df <- terra::as.data.frame(r, xy = TRUE)
      }
      # --- End CRS Check --- 

      # Rename columns directly to the final required schema
      names(df) <- c("longitude", "latitude", "intensity") 

      # Filter out NA values
      df <- df[stats::complete.cases(df), ]

      # Optional: Add a date column if provided
      if (!is.null(date)) {
        # Ensure date is in Date format
        if (!inherits(date, "Date")) {
          date <- as.Date(date)
        }
        df$date <- date
      }

      return(df)
    },
    error = function(e) {
      message(paste("Error in raster_to_points_df:", e$message)) # Use message instead of log_error
      return(data.frame()) # Return empty data frame on error
    }
  )
}

#' Convert Cached TIFFs to Partitioned Parquet
convert_tiffs_to_parquet <- function(target_date, force_update = FALSE) {
  date_str <- format(target_date, "%Y-%m-%d")
  message(sprintf("\n--- Starting Parquet Conversion for Date: %s ---", date_str))

  # Find TIFF files specifically for the target date in the cache
  # (Assuming filenames might contain the date, or we filter by date later if needed)
  # Let's list all relevant TIFs first, then process
  all_tiffs_in_cache <- fs::dir_ls(RTBM_CACHE_PATH, regexp = "_occurrences\\.tif$", ignore.case = TRUE)

  if (length(all_tiffs_in_cache) == 0) {
    message("No '*_occurrences.tif' files found in cache: ", RTBM_CACHE_PATH)
    return(invisible(NULL))
  }

  processed_count <- 0
  skipped_count <- 0
  error_count <- 0

  # Process each TIFF file found
  for (tif_path in all_tiffs_in_cache) {
    tryCatch(
      {
        # Extract species name (handles underscores vs spaces if needed)
        file_name <- fs::path_file(tif_path)
        # Assumes format 'Species_Name_occurrences.tif'
        species_name_match <- stringr::str_match(file_name, "^(.*?)_occurrences\\.tif$")
        if (is.na(species_name_match[1, 2])) {
          stop(paste("Could not extract species name from:", file_name))
        }
        scientific_name <- species_name_match[1, 2]

        # --- Hive Partitioning ---
        # Create species directory
        species_partition <- paste0("species=", scientific_name)
        species_dir <- fs::path(RTBM_PARQUET_PATH, species_partition)
        fs::dir_create(species_dir, recurse = TRUE)

        # Define final parquet file path with date partition
        date_partition <- paste0("date=", date_str)
        parquet_file_path <- fs::path(species_dir, paste0(date_partition, ".parquet"))
        # --- End Hive Partitioning ---

        # Skip if Parquet file already exists (and force_update is FALSE - though not used)
        if (fs::file_exists(parquet_file_path) && !force_update) {
          # message(sprintf("Skipping conversion for %s on %s (Parquet exists)", scientific_name, date_str))
          skipped_count <- skipped_count + 1
          next # Move to the next file
        }

        # Load raster data
        r <- terra::rast(tif_path)

        # Convert raster using the local helper function
        points_df <- raster_to_points_df(r, date = target_date)

        # Only write if data exists (handles empty rasters / cleaning)
        if (nrow(points_df) > 0) {
          # Add species column before writing
          points_df$species <- scientific_name

          # Write to Parquet file
          arrow::write_parquet(points_df, parquet_file_path)
          processed_count <- processed_count + 1
        } else {
          # message(sprintf("Skipping write for %s on %s (No data points)", scientific_name, date_str))
          # Technically skipped, but could be considered processed with no output
          skipped_count <- skipped_count + 1 # Or adjust logic if needed
        }
      },
      error = function(e) {
        message(sprintf("ERROR converting %s for %s: %s", scientific_name, date_str, e$message))
        error_count <- error_count + 1
      }
    )
  } # End loop through TIFF files

  # Final summary message
  message(sprintf("Parquet Conversion Summary for %s:", date_str))
  message(sprintf("- Files Processed & Written: %d", processed_count))
  message(sprintf("- Files Skipped (Exists or No Data): %d", skipped_count))
  message(sprintf("- Errors: %d", error_count))

  return(invisible(NULL))
}

# --- Main Execution Loop ---

total_dates <- length(date_sequence)
overall_start_time <- Sys.time()

message(sprintf("\n=== Starting Update Process for %d Date(s) ===", total_dates))

for (i in seq_along(date_sequence)) {
  current_target_date <- date_sequence[i]
  date_str_log <- format(current_target_date, "%Y-%m-%d")
  message(sprintf("\n=== Processing Date %d/%d: %s ===", i, total_dates, date_str_log))
  date_start_time <- Sys.time()

  # Step 1: Fetch and cache TIFFs for the current date
  message("\nStep 1: Fetching and Caching TIFF files...")
  tryCatch(
    {
      scientific_names <- load_scientific_names()
      if (is.null(scientific_names) || length(scientific_names) == 0) {
        message("Could not load scientific names. Skipping download.")
        return(invisible(NULL))
      }
      fetch_and_cache_tiffs(date_str_log, scientific_names, force_update = FALSE)
      message("Step 1: Fetching TIFFs for ", date_str_log, " completed successfully.")
    },
    error = function(e) {
      message(sprintf("Step 1: Error during TIFF fetching/caching for %s: %s", date_str_log, e$message))
    }
  )

  # Step 2: Convert TIFFs to Parquet for the current date
  message("\nStep 2: Converting TIFFs to Parquet...")
  tryCatch(
    {
      convert_tiffs_to_parquet(target_date = current_target_date, force_update = opts$force)
      message(sprintf("Step 2: Converting TIFFs to Parquet for %s completed.", date_str_log))
    },
    error = function(e) {
      message(sprintf("Step 2: Error during Parquet conversion for %s: %s", date_str_log, e$message))
    }
  )

  date_end_time <- Sys.time()
  message(sprintf(
    "=== Finished Processing Date %s (Duration: %.2f seconds) ===",
    date_str_log, difftime(date_end_time, date_start_time, units = "secs")
  ))
}

overall_end_time <- Sys.time()
message(sprintf(
  "\n=== Update Process Completed (Total Duration: %.2f minutes) ===",
  difftime(overall_end_time, overall_start_time, units = "mins")
))

# Optional: Add step for updating bird_info.json if needed here
# message("\nStep 3: Updating bird info JSON...")

#!/usr/bin/env Rscript

# Description:
# This script cleans the RTBM Parquet data generated by `update_rtbm_parquet_data.R`.
# It checks for and removes:
# 1. Parquet files that cannot be read.
# 2. Parquet files that are empty (0 rows).
# 3. Parquet files lacking required columns (longitude, latitude, intensity, date).
# 4. Parquet files with no valid (non-NA, positive) intensity values.
# 5. Empty species directories resulting from file cleaning.
# This script is intended to be run after the update script, potentially
# as part of the same scheduled job or separately.

# --- Libraries ---
library(arrow)
library(fs)
library(config)
library(purrr)
library(dplyr)
library(sf)

# --- Configuration & Setup ---
message("--- Script Start: RTBM Parquet Cleaning - ", Sys.time(), " ---")

# Load configuration
cfg <- config::get()
if (is.null(cfg$data_path)) {
  stop("Error: 'data_path' not found in configuration.")
}
BASE_DATA_PATH <- cfg$data_path
RTBM_PARQUET_PATH <- file.path(BASE_DATA_PATH, "rtbm", "parquet")

if (!dir_exists(RTBM_PARQUET_PATH)) {
  message("Parquet data path does not exist: ", RTBM_PARQUET_PATH)
  message("Exiting cleaning script.")
  quit(save = "no", status = 0)
}

# --- Function Definition ---

#' Clean RTBM Parquet Data
clean_rtbm_parquet_data <- function() {
  message("Starting RTBM parquet cleaning process in: ", RTBM_PARQUET_PATH)

  species_dirs <- dir_ls(RTBM_PARQUET_PATH, type = "directory")
  message(sprintf("Found %d potential species directories.", length(species_dirs)))

  total_files_checked <- 0
  total_files_removed <- 0
  total_dirs_removed <- 0

  if (length(species_dirs) == 0) {
    message("No species directories found to clean.")
    return(invisible(NULL))
  }

  for (species_dir in species_dirs) {
    species_name <- path_file(species_dir)
    message(sprintf("\nProcessing directory: %s", species_name))

    # List parquet files (adjust pattern if necessary, e.g. nested date dirs)
    # This assumes files like 'species=X/date=Y.parquet'
    parquet_files <- dir_ls(species_dir, glob = "*.parquet", recurse = FALSE, type = "file")
    files_in_dir <- length(parquet_files)
    valid_files_in_dir <- 0
    removed_files_in_dir <- 0
    results_list <- list() # Initialize list to store results for this directory

    message(sprintf("Found %d parquet files.", files_in_dir))

    if (files_in_dir == 0) {
      message("Directory contains no parquet files. Skipping.")
      next
    }

    for (file_path in parquet_files) {
      total_files_checked <- total_files_checked + 1
      file_name <- path_file(file_path)
      message(sprintf(" Checking file: %s", file_name))
      invalid_reason <- NULL
      modification_made <- FALSE # Flag to track if we changed the data
      file_status <- list(file = file_name, status = "unknown", modified_successfully = FALSE)

      tryCatch(
        {
          df <- NULL
          # Safely read the parquet file
          safe_read <- safely(read_parquet)
          read_result <- safe_read(file_path)

          if (!is.null(read_result$error)) {
            invalid_reason <- paste("Read error:", read_result$error$message)
          } else {
            df <- read_result$result

            # --- Standardization & Conversion --- START ---
            original_colnames <- names(df)
            needs_rewrite <- FALSE

            # 1. Check and Standardize Column Names
            if ("x" %in% names(df) && "y" %in% names(df) && "value" %in% names(df)) {
              message("  Detected old column format (x, y, value). Renaming...")
              df <- df |>
                rename(
                  longitude = x,
                  latitude = y,
                  intensity = value
                )
              needs_rewrite <- TRUE
            }

            # 2. Check Coordinates (only if standard columns exist now)
            if ("longitude" %in% names(df) && "latitude" %in% names(df)) {
              coord_range <- range(c(df$longitude, df$latitude), na.rm = TRUE)
              # Heuristic: If min coord is > 1000, assume it's meters (EPSG:3857)
              if (!anyNA(coord_range) && coord_range[1] > 1000) {
                message(sprintf("  Coordinates range (%.1f, %.1f) suggests meters. Attempting conversion to degrees (EPSG:4326)...", coord_range[1], coord_range[2]))
                tryCatch(
                  {
                    points_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 3857, remove = FALSE) # Keep original cols
                    points_sf_wgs84 <- st_transform(points_sf, 4326)
                    coords_wgs84 <- st_coordinates(points_sf_wgs84)
                    df$longitude <- coords_wgs84[, 1]
                    df$latitude <- coords_wgs84[, 2]
                    message("  Coordinate conversion successful.")
                    needs_rewrite <- TRUE
                  },
                  error = function(e_conv) {
                    message("  ERROR during coordinate conversion: ", e_conv$message)
                    # Mark as invalid if conversion fails
                    invalid_reason <- "Coordinate conversion failed."
                  }
                )
              } else {
                message(sprintf("  Coordinates range (%.1f, %.1f) looks like degrees. No conversion needed.", coord_range[1], coord_range[2]))
              }
            } else if (!("longitude" %in% names(df) && "latitude" %in% names(df))) {
              # This case should only happen if the initial read had neither x,y nor lon,lat
              message("  Skipping coordinate check as standard coordinate columns are missing.")
            }
            # --- Standardization & Conversion --- END ---

            # Proceed with validation checks ONLY IF conversion didn't fail
            if (is.null(invalid_reason)) {
              # Check 1: Empty file
              if (nrow(df) == 0) {
                invalid_reason <- "File is empty (0 rows)."
              } else {
                # Check 2: Required columns (after potential rename)
                required_cols <- c("longitude", "latitude", "intensity", "date")
                missing_cols <- setdiff(required_cols, names(df))
                if (length(missing_cols) > 0) {
                  # Add original colnames to message if rename happened
                  orig_info <- if (needs_rewrite) paste("(Original cols:", paste(original_colnames, collapse = ", "), ")") else ""
                  invalid_reason <- paste("Missing required columns:", paste(missing_cols, collapse = ", "), orig_info)
                } else {
                  # Check 3: Valid Coordinates (WGS84 range) - Final check after potential conversion
                  lon_range <- range(df$longitude, na.rm = TRUE)
                  lat_range <- range(df$latitude, na.rm = TRUE)
                  if (anyNA(lon_range) || anyNA(lat_range) || lon_range[1] < -180 || lon_range[2] > 180 || lat_range[1] < -90 || lat_range[2] > 90) {
                    invalid_reason <- sprintf(
                      "Invalid final coordinate range (Lon: %.2f to %.2f, Lat: %.2f to %.2f). Expected WGS84 degrees.",
                      lon_range[1], lon_range[2], lat_range[1], lat_range[2]
                    )
                  } else {
                    # Check 4: Valid intensity values
                    valid_intensities <- df$intensity[!is.na(df$intensity)]
                    if (length(valid_intensities) == 0) {
                      invalid_reason <- "No valid (non-NA) intensity values found."
                    } else {
                      # Check 5: Positive intensity values
                      positive_intensities <- valid_intensities[valid_intensities > 0]
                      if (length(positive_intensities) == 0) {
                        invalid_reason <- "No positive intensity values found."
                      }
                    }
                  }
                }
              }
            }
          }
        },
        error = function(e) {
          # Catch any unexpected error during checks
          invalid_reason <- paste("Unexpected check error:", e$message)
          file_status$status <<- "error_unexpected"
        }
      )

      # --- Write back if modified and still valid ---
      if (needs_rewrite && is.null(invalid_reason)) {
        message("  File was modified (columns/coords). Writing changes back...")
        tryCatch(
          {
            write_parquet(df, file_path)
            message("  Successfully wrote back corrected file.")
            modification_made <- TRUE # Record that we overwrote it
          },
          error = function(e_write) {
            message("  ERROR writing back corrected file: ", e_write$message)
            # If write back fails, treat the original file as invalid
            invalid_reason <- "Failed to write back corrected data."
            # Attempt to delete the potentially corrupted file we tried to write? Or leave original?
            # Let's mark for deletion based on the original check result.
          }
        )
      }

      # Perform removal if needed
      if (!is.null(invalid_reason)) {
        message(sprintf("  Reason for removal: %s", invalid_reason))
        # Avoid deleting if we successfully modified and wrote it back in this run?
        # No, if invalid_reason is set, it means either initial read failed,
        # conversion failed, validation failed, or write back failed.
        # So, deletion is appropriate.
        message(sprintf("  Removing invalid file: %s", file_name))
        tryCatch(
          {
            file_delete(file_path)
            total_files_removed <- total_files_removed + 1
            removed_files_in_dir <- removed_files_in_dir + 1
            file_status$status <- "removed"
          },
          error = function(e) {
            message(sprintf("  ERROR: Failed to remove file %s: %s", file_name, e$message))
            file_status$status <- "error_removal"
          }
        )
      } else {
        message(sprintf("  File is valid."))
        valid_files_in_dir <- valid_files_in_dir + 1
        file_status$status <- ifelse(modification_made, "modified_successfully", "valid")
        file_status$modified_successfully <- modification_made
      }
      results_list[[length(results_list) + 1]] <- file_status # Append result
    } # End file loop

    modified_count <- sum(map_lgl(results_list, `[[`, "modified_successfully"))
    message(sprintf(
      "\nFinished directory %s. Total: %d, Valid (or corrected): %d, Removed: %d, Modified & Saved: %d",
      species_name, files_in_dir, valid_files_in_dir, removed_files_in_dir, modified_count
    ))

    # Remove directory if empty
    remaining_files_count <- files_in_dir - removed_files_in_dir
    if (remaining_files_count == 0) {
      message(sprintf("Removing now empty directory: %s", species_name))
      tryCatch(
        {
          dir_delete(species_dir)
          total_dirs_removed <- total_dirs_removed + 1
        },
        error = function(e) {
          message(sprintf("ERROR: Failed to remove empty directory %s: %s", species_name, e$message))
        }
      )
    } else {
      message(sprintf("Finished processing %s. Valid files remaining: %d", species_name, remaining_files_count))
    }
  } # End directory loop

  message(sprintf("\n--- Cleaning Summary ---"))
  message(sprintf("Total files checked: %d", total_files_checked))
  message(sprintf("Total invalid files removed: %d", total_files_removed))
  message(sprintf("Total empty directories removed: %d", total_dirs_removed))
  message(sprintf("-------------------------"))

  invisible(NULL)
}

# --- Main Execution ---
tryCatch(
  {
    clean_rtbm_parquet_data()
  },
  error = function(e) {
    message("\nFATAL ERROR during cleaning process: ", e$message)
    # Consider exiting with non-zero status for scheduler
    quit(save = "no", status = 1)
  }
)

message("--- Script End: RTBM Parquet Cleaning - ", Sys.time(), " ---")

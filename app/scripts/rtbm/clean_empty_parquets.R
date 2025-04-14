#!/usr/bin/env Rscript

# Script to clean invalid RTBM parquet files
# This script will identify and (optionally) remove invalid parquet files

box::use(
  arrow[read_parquet, write_parquet],
  dplyr[rename, filter],
  sf[st_as_sf, st_transform, st_coordinates],
  fs[file_delete, dir_delete],
)

#' Clean empty or invalid parquet files
#' @export
clean_empty_parquets <- function() {
  cat("Starting parquet file cleaning process...\n")

  # Get all species directories
  species_dirs <- list.dirs("app/data/rtbm/parquet", recursive = FALSE)
  cat(sprintf("Found %d species directories\n", length(species_dirs)))

  for (species_dir in species_dirs) {
    cat(sprintf("\nProcessing directory: %s\n", basename(species_dir)))

    # Get all parquet files in the species directory
    parquet_files <- list.files(species_dir, pattern = "\\.parquet$", full.names = TRUE)
    valid_files <- 0

    cat(sprintf("Found %d parquet files\n", length(parquet_files)))

    for (file_path in parquet_files) {
      cat(sprintf("\nChecking file: %s\n", basename(file_path)))

      tryCatch(
        {
          invalid_file <- FALSE

          # Read parquet file
          df <- read_parquet(file_path)
          cat(sprintf("Loaded file with %d rows and %d columns\n", nrow(df), ncol(df)))

          # Check and standardize column names for both old and new formats
          if ("x" %in% names(df) && "y" %in% names(df) && "value" %in% names(df)) {
            # Old format with x, y, value
            cat("Found old column format (x,y,value)\n")

            # Convert coordinates if they're in meters (likely EPSG:3857)
            coord_range <- range(c(df$x, df$y), na.rm = TRUE)
            if (coord_range[1] > 1000) {
              cat("Converting coordinates from EPSG:3857 to EPSG:4326\n")
              # Convert to sf object with Web Mercator projection
              points_sf <- st_as_sf(df, coords = c("x", "y"), crs = 3857)
              # Transform to WGS84
              points_sf <- st_transform(points_sf, 4326)
              # Extract coordinates
              coords <- st_coordinates(points_sf)
              # Update dataframe with transformed coordinates
              df$x <- coords[, 1]
              df$y <- coords[, 2]
            }

            cat("Standardizing column names (x,y,value -> longitude,latitude,intensity)\n")
            df <- df |>
              rename(
                longitude = x,
                latitude = y,
                intensity = value
              )
          } else if ("longitude" %in% names(df) && "latitude" %in% names(df) && "intensity" %in% names(df)) {
            # New format - already has correct column names
            cat("Found new column format (longitude,latitude,intensity)\n")

            # --- Explicit Coordinate Check ---
            cat("Checking coordinates for potential conversion...\n")
            # Check if coordinates need conversion (although they shouldn't in new format)
            coord_range <- range(c(df$longitude, df$latitude), na.rm = TRUE)
            cat(sprintf("Coordinate range detected: min=%.2f, max=%.2f\n", coord_range[1], coord_range[2]))

            if (coord_range[1] > 1000) {
              cat("Coordinate range > 1000. Attempting conversion from EPSG:3857 to EPSG:4326...\n")
              tryCatch(
                {
                  points_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 3857)
                  points_sf <- st_transform(points_sf, 4326)
                  coords <- st_coordinates(points_sf)
                  df$longitude <- coords[, 1]
                  df$latitude <- coords[, 2]
                  cat("Coordinate conversion successful.\n")
                },
                error = function(e) {
                  cat(sprintf("ERROR during coordinate conversion: %s\n", e$message))
                  # Decide if this should make the file invalid
                  # invalid_file <- TRUE
                }
              )
            } else {
              cat("Coordinate range <= 1000. No conversion needed.\n")
            }
            # --------------------------------
          } else {
            invalid_file <- TRUE
            cat("ERROR: Invalid column format\n")
            cat(sprintf("Available columns: %s\n", paste(names(df), collapse = ", ")))
          }

          # Validate required columns exist
          required_cols <- c("longitude", "latitude", "intensity")
          if (!all(required_cols %in% names(df))) {
            invalid_file <- TRUE
            cat("ERROR: Missing required columns\n")
            cat(sprintf("Available columns: %s\n", paste(names(df), collapse = ", ")))
          } else {
            # Check if there are any non-NA values in intensity
            valid_intensities <- df$intensity[!is.na(df$intensity)]

            if (length(valid_intensities) == 0) {
              invalid_file <- TRUE
              cat("ERROR: No valid intensity values found\n")
            } else {
              # Check for positive values
              positive_intensities <- valid_intensities[valid_intensities > 0]
              if (length(positive_intensities) == 0) {
                invalid_file <- TRUE
                cat("ERROR: No positive intensity values found\n")
              }
            }
          }

          if (invalid_file) {
            cat(sprintf("Removing invalid file: %s\n", basename(file_path)))
            file_delete(file_path)
          } else {
            # Write back standardized data
            write_parquet(df, file_path)
            cat("File standardized and saved successfully\n")
            valid_files <- valid_files + 1
          }
        },
        error = function(e) {
          cat(sprintf("ERROR processing file: %s\n", basename(file_path)))
          cat(sprintf("Error message: %s\n", e$message))
          file_delete(file_path)
        }
      )
    }

    # Check if directory is empty after processing
    remaining_files <- list.files(species_dir, pattern = "\\.parquet$")
    if (length(remaining_files) == 0) {
      cat(sprintf("Removing empty directory: %s\n", basename(species_dir)))
      dir_delete(species_dir)
    } else {
      cat(sprintf(
        "Successfully processed %d valid files in %s\n",
        valid_files, basename(species_dir)
      ))
    }
  }
  cat("\nParquet file cleaning process completed.\n")
}

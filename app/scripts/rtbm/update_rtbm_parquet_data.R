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
library(sf) # Added for CRS projection
library(stats) # Added for complete.cases used in raster_to_points_df
library(xml2) # Added for parsing XML bucket listing
library(parallel) # Added for parallel processing

# --- Standalone Configuration ---
# Define paths relative to the project root assuming the script is run from there.
# Adjust if the script's working directory is different.
PROJECT_ROOT <- getwd() # Assuming script is run from project root
RTBM_CACHE_PATH <- fs::path(PROJECT_ROOT, "app", "data", "rtbm", "cache")
RTBM_PARQUET_PATH <- fs::path(PROJECT_ROOT, "app", "data", "rtbm", "parquet")
BIRD_INFO_JSON_PATH <- fs::path(PROJECT_ROOT, "app", "data", "rtbm", "bird_info.json")
BIRD_INFO_URL <- "https://bird-photos.a3s.fi/bird_info.json" # Added URL for download

# !!! IMPORTANT: Replace this placeholder with the actual base URL for the TIFF files !!!
RTBM_TIFF_BUCKET_URL_BASE <- "https://2007581-webportal.a3s.fi/daily/"
# --- End Standalone Configuration ---

# --- Configuration & Setup ---
message("--- Script Start: RTBM Parquet Update - ", Sys.time(), " ---")

# Default start date if no specific date is provided
DEFAULT_START_DATE <- "2025-01-16"

# Create directories if they don't exist
if (!dir_exists(RTBM_CACHE_PATH)) dir_create(RTBM_CACHE_PATH, recurse = TRUE)
if (!dir_exists(RTBM_PARQUET_PATH)) dir_create(RTBM_PARQUET_PATH, recurse = TRUE)

# --- Argument Parsing --- START ---
option_list <- list(
  make_option(c("-d", "--date"),
    type = "character", default = NULL,
    help = "Target date in YYYY-MM-DD format. If omitted, processes from --start-date (or default) to --end-date (or yesterday). Overrides --start-date and --end-date.",
    metavar = "YYYY-MM-DD"
  ),
  # Reverted back to --force, default FALSE (meaning use cache by default)
  make_option(c("-f", "--force"),
    action = "store_true", default = FALSE,
    help = "Force re-download of TIFF files and overwrite existing Parquet files"
  ),
  # Add start and end date options
  make_option(c("-s", "--start-date"),
    type = "character", default = NULL,
    help = "Start date for processing range (YYYY-MM-DD). Defaults to internal DEFAULT_START_DATE if omitted.",
    metavar = "YYYY-MM-DD"
  ),
  make_option(c("-e", "--end-date"),
    type = "character", default = NULL,
    help = "End date for processing range (YYYY-MM-DD). Defaults to yesterday if omitted.",
    metavar = "YYYY-MM-DD"
  )
)

opt_parser <- OptionParser(option_list = option_list)
opts <- parse_args(opt_parser)

# Determine target date(s)
date_sequence <- NULL

if (!is.null(opts$date)) {
  # Highest priority: Specific date provided
  parsed_date <- ymd(opts$date, quiet = TRUE)
  if (!is.na(parsed_date)) {
    date_sequence <- c(parsed_date)
    message(sprintf("Processing specified target date: %s", format(date_sequence[1], "%Y-%m-%d")))
  } else {
    stop(sprintf("Invalid format for --date ('%s'). Please use YYYY-MM-DD.", opts$date))
  }
} else {
  # No specific date, determine range
  start_date_input <- opts$`start-date`
  end_date_input <- opts$`end-date`

  # Determine start date
  if (!is.null(start_date_input)) {
    start_date <- ymd(start_date_input, quiet = TRUE)
    if (is.na(start_date)) {
      stop(sprintf("Invalid format for --start-date ('%s'). Please use YYYY-MM-DD.", start_date_input))
    }
  } else {
    start_date <- ymd(DEFAULT_START_DATE, quiet = TRUE)
    if (is.na(start_date)) {
      stop(sprintf("Invalid internal DEFAULT_START_DATE: %s", DEFAULT_START_DATE))
    }
  }

  # Determine end date
  if (!is.null(end_date_input)) {
    end_date <- ymd(end_date_input, quiet = TRUE)
    if (is.na(end_date)) {
      stop(sprintf("Invalid format for --end-date ('%s'). Please use YYYY-MM-DD.", end_date_input))
    }
  } else {
    end_date <- Sys.Date() - days(1)
  }

  # Validate and create sequence
  if (start_date > end_date) {
    stop(sprintf(
      "Calculated start date (%s) is after end date (%s). Check inputs or system date.",
      format(start_date, "%Y-%m-%d"), format(end_date, "%Y-%m-%d")
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
    message("Local bird_info.json not found at ", BIRD_INFO_JSON_PATH, ". Attempting download...")
    tryCatch(
      {
        download_result <- download.file(BIRD_INFO_URL, BIRD_INFO_JSON_PATH, mode = "wb", quiet = TRUE)
        if (download_result != 0) {
          # Non-zero status often indicates an error
          stop("Download failed with status: ", download_result)
        }
        if (!file_exists(BIRD_INFO_JSON_PATH)) {
          # Check again if file actually exists after download attempt
          stop("File still does not exist after download attempt.")
        }
        message("Successfully downloaded bird_info.json to ", BIRD_INFO_JSON_PATH)
      },
      error = function(e) {
        message("Error downloading bird_info.json from ", BIRD_INFO_URL, ": ", e$message)
        # Attempt to clean up potentially incomplete file
        if (file_exists(BIRD_INFO_JSON_PATH)) file.remove(BIRD_INFO_JSON_PATH)
        return(NULL) # Return NULL if download fails
      }
    )
  }

  # Proceed to read the file (either pre-existing or just downloaded)
  if (!file_exists(BIRD_INFO_JSON_PATH)) {
    message("Error: bird_info.json still not available after download attempt.")
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

#' Fetches the complete list of keys from the S3 bucket using pagination.
#' @param xml_list_url The base URL for the bucket listing.
#' @return A character vector of all keys found in the bucket, or NULL on error.
fetch_all_keys <- function(xml_list_url = "https://2007581-webportal.a3s.fi") {
  message("Fetching complete key listing from: ", xml_list_url)

  # Initialize variables for pagination
  all_keys <- character(0)
  is_truncated <- TRUE
  marker <- NULL
  page_count <- 0
  start_time <- Sys.time()
  success <- TRUE

  tryCatch(
    {
      # Loop to handle pagination
      while (is_truncated) {
        page_count <- page_count + 1
        req <- httr2::request(xml_list_url)

        # Add marker parameter if we're not on the first page
        if (!is.null(marker)) {
          # message(sprintf("Using marker: %s", marker))
          req <- httr2::req_url_query(req, marker = marker)
        }

        # Perform the request with error handling
        resp <- tryCatch(
          {
            httr2::req_perform(req)
          },
          error = function(e) {
            message("Error fetching XML listing page: ", e$message)
            return(NULL) # Return NULL to indicate failure
          }
        )

        # Check for request failure
        if (is.null(resp)) {
          message("Request failed. Breaking pagination loop.")
          success <- FALSE
          break
        }

        if (httr2::resp_status(resp) != 200) {
          message(paste("Failed to fetch XML listing page. Status:", httr2::resp_status(resp)))
          success <- FALSE
          break
        }

        # Try to parse XML content
        tryCatch(
          {
            xml_content <- httr2::resp_body_xml(resp)

            # Handle possible namespace issues
            key_patterns <- list(
              ".//Key", ".//*[local-name()='Key']", ".//d1:Key", ".//s3:Key"
            )

            current_keys <- character(0)
            used_pattern <- "N/A"
            for (pattern in key_patterns) {
              key_nodes <- xml2::xml_find_all(xml_content, pattern)
              if (length(key_nodes) > 0) {
                current_keys <- xml2::xml_text(key_nodes)
                used_pattern <- pattern
                break
              }
            }
            # message(sprintf("Page %d: Found %d keys (pattern: %s)", page_count, length(current_keys), used_pattern))

            all_keys <- c(all_keys, current_keys)

            # Similar approach for IsTruncated
            is_truncated <- FALSE
            is_truncated_patterns <- list(
              ".//IsTruncated", ".//*[local-name()='IsTruncated']", ".//d1:IsTruncated", ".//s3:IsTruncated"
            )

            for (pattern in is_truncated_patterns) {
              truncated_node <- xml2::xml_find_first(xml_content, pattern)
              if (!is.na(truncated_node)) {
                is_truncated_text <- xml2::xml_text(truncated_node)
                is_truncated <- tolower(is_truncated_text) == "true"
                # message(sprintf("IsTruncated: %s (pattern: %s)", is_truncated, pattern))
                break
              }
            }

            if (is_truncated) {
              token_patterns <- list(
                list(token = ".//NextContinuationToken", marker = ".//NextMarker"),
                list(token = ".//*[local-name()='NextContinuationToken']", marker = ".//*[local-name()='NextMarker']"),
                list(token = ".//d1:NextContinuationToken", marker = ".//d1:NextMarker"),
                list(token = ".//s3:NextContinuationToken", marker = ".//s3:NextMarker")
              )

              marker <- NULL

              for (pattern_pair in token_patterns) {
                next_token_node <- xml2::xml_find_first(xml_content, pattern_pair$token)
                if (!is.na(next_token_node)) {
                  marker <- xml2::xml_text(next_token_node)
                  # message("Using NextContinuationToken")
                  break
                }
                next_marker_node <- xml2::xml_find_first(xml_content, pattern_pair$marker)
                if (!is.na(next_marker_node)) {
                  marker <- xml2::xml_text(next_marker_node)
                  # message("Using NextMarker")
                  break
                }
              }

              if (is.null(marker) && length(current_keys) > 0) {
                marker <- current_keys[length(current_keys)]
                # message("Using last key as marker")
              }

              if (is.null(marker)) {
                message("Warning: IsTruncated is true but no pagination marker/token found. Breaking loop.")
                is_truncated <- FALSE
              }
            }
          },
          error = function(e) {
            message("Error processing XML response page: ", e$message)
            success <- FALSE
            is_truncated <<- FALSE # Force loop exit on error
          }
        ) # End tryCatch for XML processing
      } # End while loop
    },
    error = function(e) {
      message("Unexpected error during XML fetching: ", e$message)
      success <- FALSE
    }
  )

  end_time <- Sys.time()
  duration <- round(as.numeric(difftime(end_time, start_time, units = "secs")), 2)

  if (success) {
    message(sprintf(
      "Fetched %d total keys across %d pages in %.2f seconds.",
      length(all_keys), page_count, duration
    ))
    return(all_keys)
  } else {
    message(sprintf(
      "Failed to fetch complete key list after %d pages and %.2f seconds.",
      page_count, duration
    ))
    return(NULL)
  }
}

#' Fetch and cache TIFF files for a specific date from an S3 bucket.
#' @param target_date The date for which to fetch files (Date object).
#' @param all_keys A character vector containing all keys from the bucket.
#' @param force_download If TRUE, redownload files even if they exist in cache.
#' @return Invisible NULL. Side effect is downloading files to RTBM_CACHE_PATH.
fetch_and_cache_tiffs <- function(target_date, all_keys, force_download = FALSE) {
  if (is.null(all_keys)) {
    message("Error: Invalid list of keys provided. Skipping fetch for ", format(target_date, "%Y-%m-%d"))
    return(invisible(NULL))
  }

  date_str <- format(target_date, "%Y-%m-%d")
  message(sprintf("--- Inside fetch_and_cache_tiffs for date: %s ---", date_str))

  # Load required species names
  # names_to_process <- load_scientific_names()
  # if (is.null(names_to_process)) {
  #   message("Failed to load species names. Cannot process files.")
  #   return(invisible(NULL))
  # }
  # message("Number of names to process: ", length(names_to_process))
  # message("First few names: ", paste(head(names_to_process), collapse=", "))

  xml_list_url <- "https://2007581-webportal.a3s.fi"
  downloaded_count <- 0
  cached_count <- 0
  skipped_count <- 0
  error_count <- 0

  # --- Remove Internal XML Fetching Logic ---
  # The following block that fetched XML is removed, as keys are now passed in.
  # tryCatch(... while(is_truncated) {...} ...)
  # ---

  # Filter the pre-fetched keys relevant to the target date
  date_pattern <- paste0("^daily/", date_str, "/")
  relevant_keys <- all_keys[stringr::str_detect(all_keys, date_pattern)]
  message(sprintf("Found %d keys matching date %s.", length(relevant_keys), date_str))

  if (length(relevant_keys) == 0) {
    message("No files found for date ", date_str, " in the provided key list.")
    # Update summary and return
    message("--- Fetch/Cache Summary for ", date_str, " ---")
    message("  - Files downloaded: 0")
    message("  - Files found in cache: 0")
    message("  - Files skipped (species not required): 0")
    message("  - Errors encountered: 0")
    message("--- End Summary for ", date_str, " ---")
    return(invisible(NULL))
  }

  # Ensure the cache directory exists
  if (!fs::dir_exists(RTBM_CACHE_PATH)) {
    dir_create(RTBM_CACHE_PATH, recurse = TRUE)
  }

  for (key in relevant_keys) {
    # Extract the filename from the key
    # Assumes format 'daily/YYYY-MM-DD/filename.tif'
    file_name_in_url <- basename(key)

    # We're downloading all files for this date, regardless of species name
    # Note: We keep the code to extract species for reference/debug if needed
    species_name_from_key <- stringr::str_replace(tools::file_path_sans_ext(file_name_in_url), "_occurrences", "")
    # message(sprintf("Processing file: %s (extracted name: %s)", file_name_in_url, species_name_from_key))

    # Construct local file path (use original filename with underscores)
    # Use file_name_in_url directly which has the underscores
    local_file_name <- file_name_in_url
    target_local_path <- file.path(RTBM_CACHE_PATH, local_file_name)

    # Check cache
    file_exists_check <- file.exists(target_local_path)

    if (is.na(file_exists_check)) {
      warning(paste("Could not determine existence for:", target_local_path, "- file.exists returned NA. Treating as not cached."))
      file_exists_check <- FALSE
    }

    if (file_exists_check && !force_download) {
      # message("  - Found in cache: ", local_file_name)
      cached_count <- cached_count + 1
      next # Skip download, already cached
    }

    # Construct full URL for download
    # Assuming xml_list_url is the base endpoint, and key is the path
    target_url <- paste0(xml_list_url, "/", key)

    # Attempt download
    tryCatch(
      {
        # message("  - Attempting download: ", target_url)
        download_status <- utils::download.file(target_url, target_local_path, mode = "wb", quiet = TRUE)

        if (download_status == 0) {
          # message("    - Download successful: ", local_file_name)
          downloaded_count <- downloaded_count + 1
        } else {
          warning(paste("Download failed for", target_url, "with status code:", download_status))
          error_count <- error_count + 1
          # Attempt to remove potentially incomplete file
          if (file.exists(target_local_path)) {
            try(file.remove(target_local_path), silent = TRUE)
          }
        }
      },
      error = function(e) {
        warning(paste("Error downloading", target_url, ":", e$message))
        error_count <- error_count + 1
        # Attempt to remove potentially incomplete file
        if (file.exists(target_local_path)) {
          try(file.remove(target_local_path), silent = TRUE)
        }
      }
    )
  } # End loop through relevant keys

  # --- Summarization Logic ---
  message("--- Fetch/Cache Summary for ", date_str, " ---")
  message("  - Files downloaded: ", downloaded_count)
  message("  - Files found in cache: ", cached_count)
  message("  - Files skipped (species not required): ", skipped_count)
  message("  - Errors encountered: ", error_count)
  message("--- End Summary for ", date_str, " ---")
  # --- End Summarization Logic ---

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
        grepl("UNIT[\"']degree[\"']", terra::crs(r, describe = TRUE)$proj_unit, ignore.case = TRUE)

      # Use isFALSE to safely handle NA values from the is_geographic check
      if (isFALSE(is_geographic)) {
        message(sprintf("Projecting raster from %s to %s", terra::crs(r, describe = TRUE)$name, target_crs))
        # Ensure sf is available for projection
        # requireNamespace("sf", quietly = TRUE)
        r_projected <- tryCatch(
          {
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

#' Validate, Standardize, and Clean a Single Parquet File
#' Reads a parquet file, checks/standardizes columns (x,y,value -> lon,lat,intensity),
#' converts coordinates if needed (meters to degrees), validates content,
#' and either overwrites the file with changes or removes it if invalid.
#' @param file_path Path to the Parquet file.
#' @return A list with status: 'valid', 'modified_successfully', 'removed_read_error',
#'   'removed_empty', 'removed_missing_cols', 'removed_coord_error',
#'   'removed_intensity_error', 'removed_write_error', 'error_unexpected'.
validate_and_clean_parquet_file <- function(file_path) {
  message(sprintf(" Validating/Cleaning file: %s", basename(file_path)))
  invalid_reason <- NULL
  modification_made <- FALSE
  status <- "unknown"
  needs_rewrite <- FALSE

  tryCatch(
    {
      df <- NULL
      # Safely read the parquet file
      safe_read <- purrr::safely(arrow::read_parquet)
      read_result <- safe_read(file_path)

      if (!is.null(read_result$error)) {
        invalid_reason <- paste("Read error:", read_result$error$message)
        status <- "removed_read_error"
      } else {
        df <- read_result$result
        original_colnames <- names(df)

        # 1. Check and Standardize Column Names
        if ("x" %in% names(df) && "y" %in% names(df) && "value" %in% names(df)) {
          message("  Detected old column format (x, y, value). Renaming...")
          df <- df |> dplyr::rename(
            longitude = x,
            latitude = y,
            intensity = value
          )
          needs_rewrite <- TRUE
        }

        # 2. Check Coordinates (only if standard columns exist now)
        if ("longitude" %in% names(df) && "latitude" %in% names(df)) {
          coord_range <- range(c(df$longitude, df$latitude), na.rm = TRUE)
          if (!anyNA(coord_range) && coord_range[1] > 1000) {
            message(sprintf("  Coordinates range (%.1f, %.1f) suggests meters. Converting to degrees (WGS84)...", coord_range[1], coord_range[2]))
            tryCatch(
              {
                sf_df <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 3857, remove = FALSE)
                sf_df_wgs84 <- sf::st_transform(sf_df, crs = 4326)
                coords_wgs84 <- sf::st_coordinates(sf_df_wgs84)
                df$longitude <- coords_wgs84[, "X"]
                df$latitude <- coords_wgs84[, "Y"]
                needs_rewrite <- TRUE
                message("  Coordinate conversion successful.")
              },
              error = function(e_conv) {
                message("  ERROR during coordinate conversion: ", e_conv$message)
                invalid_reason <<- paste("Coordinate conversion failed:", e_conv$message) # Assign to outer scope
                status <<- "removed_coord_error"
              }
            )
          } else {
            # message(sprintf("  Coordinates range (%.1f, %.1f) looks like degrees. No conversion needed.", coord_range[1], coord_range[2]))
          }
        } else {
          message("  Skipping coordinate check as standard coordinate columns are missing.")
          # If standard cols are missing now, they were missing initially
          if (!("longitude" %in% original_colnames || "latitude" %in% original_colnames)) {
            missing_cols_check <- setdiff(c("longitude", "latitude"), original_colnames)
            invalid_reason <- paste("Missing required columns:", paste(missing_cols_check, collapse = ", "))
            status <- "removed_missing_cols"
          }
        }

        # Proceed with validation checks ONLY IF conversion didn't fail and we don't already have a reason
        if (is.null(invalid_reason)) {
          # Check 1: Empty file
          if (nrow(df) == 0) {
            invalid_reason <- "File is empty (0 rows)."
            status <- "removed_empty"
          } else {
            # Check 2: Required columns (after potential rename)
            required_cols <- c("longitude", "latitude", "intensity", "date") # Assuming date is added before this
            missing_cols <- setdiff(required_cols, names(df))
            if (length(missing_cols) > 0) {
              orig_info <- if (needs_rewrite) paste("(Original cols:", paste(original_colnames, collapse = ", "), ")") else ""
              invalid_reason <- paste("Missing required columns:", paste(missing_cols, collapse = ", "), orig_info)
              status <- "removed_missing_cols"
            } else {
              # Check 3: Valid Coordinates (WGS84 range) - Final check
              lon_range <- range(df$longitude, na.rm = TRUE)
              lat_range <- range(df$latitude, na.rm = TRUE)
              if (anyNA(lon_range) || anyNA(lat_range) || lon_range[1] < -180 || lon_range[2] > 180 || lat_range[1] < -90 || lat_range[2] > 90) {
                invalid_reason <- sprintf("Invalid coordinate range after potential conversion (Lon: %.2f, %.2f; Lat: %.2f, %.2f).", lon_range[1], lon_range[2], lat_range[1], lat_range[2])
                status <- "removed_coord_error"
              } else {
                # Check 4: Valid Intensity
                if (!("intensity" %in% names(df))) {
                  invalid_reason <- "Missing required column: intensity"
                  status <- "removed_missing_cols" # Technically missing col
                } else {
                  valid_intensities <- !is.na(df$intensity)
                  if (sum(valid_intensities) == 0) {
                    invalid_reason <- "All intensity values are NA."
                    status <- "removed_intensity_error"
                  } else {
                    positive_intensities <- df$intensity[valid_intensities] > 0
                    if (sum(positive_intensities) == 0) {
                      invalid_reason <- "No positive intensity values found."
                      status <- "removed_intensity_error"
                    }
                  }
                }
              }
            }
          }
        }
      }
    },
    error = function(e) {
      invalid_reason <<- paste("Unexpected check error:", e$message)
      status <<- "error_unexpected"
    }
  )

  # --- Write back if modified and still valid ---
  if (needs_rewrite && is.null(invalid_reason)) {
    message("  File was modified (columns/coords). Writing changes back...")
    tryCatch(
      {
        arrow::write_parquet(df, file_path)
        message("  Successfully wrote back corrected file.")
        modification_made <- TRUE
        status <- "modified_successfully"
      },
      error = function(e_write) {
        message("  ERROR writing back corrected file: ", e_write$message)
        invalid_reason <- "Failed to write back corrected data."
        status <- "removed_write_error"
      }
    )
  }

  # --- Perform removal if needed ---
  if (!is.null(invalid_reason)) {
    message(sprintf("  Reason for removal: %s", invalid_reason))
    message(sprintf("  Removing invalid file: %s", basename(file_path)))
    tryCatch(
      {
        fs::file_delete(file_path)
        # Status is already set based on the reason for removal
      },
      error = function(e_rem) {
        message(sprintf("  ERROR: Failed to remove file %s: %s", basename(file_path), e_rem$message))
        # Keep the original removal reason status, but log the removal error
      }
    )
  } else if (status == "unknown") {
    # If no reason to remove and wasn't modified, it's valid
    status <- "valid"
    message("  File is valid.")
  } else if (status == "modified_successfully") {
    message("  File was modified and saved successfully.")
  }

  return(list(status = status, reason = invalid_reason))
}

#' Convert Cached TIFFs to Partitioned Parquet Format
#' @param target_date Date object for the day to process.
#' @param force_update Boolean, if TRUE, overwrite existing Parquet files.
#' @return Invisible NULL. Messages indicate progress and errors.
convert_tiffs_to_parquet <- function(target_date, force_update = FALSE) {
  date_str <- format(target_date, "%Y-%m-%d")
  message(sprintf("--- Starting Parquet Conversion for %s ---", date_str))

  # Find TIFF files specifically for this date
  date_pattern <- paste0("*_occurrences.tif")
  message(sprintf("Looking for TIFF files with pattern: %s", date_pattern))
  cached_files <- fs::dir_ls(RTBM_CACHE_PATH, glob = date_pattern, type = "file")
  message(sprintf("Found %d cached TIFF files to process.", length(cached_files)))

  if (length(cached_files) == 0) {
    message("No cached TIFF files found to process.")
    return(invisible(NULL))
  }

  # Print a sample of the filenames for debugging
  if (length(cached_files) > 0) {
    sample_size <- min(5, length(cached_files))
    sample_files <- cached_files[1:sample_size]
    message("Sample cached filenames:")
    for (file in sample_files) {
      message("  - ", basename(file))
    }
  }

  # We don't need to load the scientific names map anymore since we're extracting directly
  # from filenames, but we'll keep this for backward compatibility
  scientific_names_map <- load_scientific_names()
  if (is.null(scientific_names_map) || length(scientific_names_map) == 0) {
    message("Warning: Could not load scientific names mapping from bird_info.json.")
    message("Will extract species names directly from filenames.")
  }

  processed_count <- 0
  skipped_count <- 0
  error_count <- 0
  validation_removed_count <- 0
  validation_modified_count <- 0

  # Ensure Parquet base directory exists
  if (!fs::dir_exists(RTBM_PARQUET_PATH)) fs::dir_create(RTBM_PARQUET_PATH, recurse = TRUE)

  for (tiff_file_path in cached_files) {
    file_name <- basename(tiff_file_path)
    message("Processing: ", file_name)

    # Extract scientific name from filename
    # Expected format is 'Genus_species_occurrences.tif'
    if (grepl("_occurrences.tif$", file_name)) {
      # Remove '_occurrences.tif' to get 'Genus_species'
      name_part <- gsub("_occurrences.tif$", "", file_name)
      # Replace underscores with spaces for the scientific name
      scientific_name <- stringr::str_replace_all(name_part, "_", " ")
      message("  Extracted scientific name: ", scientific_name)
    } else {
      # Fallback if filename doesn't match expected pattern
      message("  Warning: Unexpected filename format: ", file_name)
      # Just use the file name without extension as species
      scientific_name <- tools::file_path_sans_ext(file_name)
    }

    # Skip species check - we want to process all files
    # if (!scientific_name %in% scientific_names_map) {
    #    message(sprintf("Skipping %s: Species '%s' not in the loaded list.", file_name, scientific_name))
    #    skipped_count <- skipped_count + 1 # Count as skipped
    #    next
    # }

    # --- Hive Partitioning --- START ---
    # Create species directory
    species_partition <- paste0("species=", scientific_name)
    species_dir <- fs::path(RTBM_PARQUET_PATH, species_partition)
    fs::dir_create(species_dir, recurse = TRUE)

    # Define final parquet file path with date partition
    date_partition <- paste0("date=", date_str)
    parquet_file_path <- fs::path(species_dir, paste0(date_partition, ".parquet"))
    # --- End Hive Partitioning ---

    # Skip if Parquet file already exists AND force_update is FALSE
    if (fs::file_exists(parquet_file_path) && !force_update) {
      # message(sprintf("Skipping conversion for %s on %s (Parquet exists)", scientific_name, date_str))
      skipped_count <- skipped_count + 1
      next # Move to the next file
    }

    # --- Conversion Logic --- START ---
    conversion_successful <- FALSE
    tryCatch(
      {
        # Load raster data
        r <- terra::rast(tiff_file_path)

        # This is a performance bottleneck according to benchmarks
        # Apply optimizations: use parallel processing for raster transformations if available
        if (!is.null(parallel_cluster)) {
          # When using parallel processing, we need to ensure the terra package is loaded in each worker
          message("  Using parallel processing for raster transformation")
          terra::terraOptions(progress = 0, memfrac = 0.5) # Reduce memory usage, turn off progress bar

          # Set threads for terra operations
          # Warning: on some systems using too many threads can cause issues
          # We'll use a modest number tied to our cluster size
          terra::terraOptions(threads = no_cores)

          # Convert raster using the local helper function with parallel processing
          points_df <- raster_to_points_df(r, date = target_date)
        } else {
          # Standard single-threaded processing
          points_df <- raster_to_points_df(r, date = target_date)
        }

        # Only write if data exists (handles empty rasters / cleaning)
        if (nrow(points_df) > 0) {
          # Add species column before writing
          points_df$species <- scientific_name # Ensure species column matches partition

          # Write to Parquet file (Overwrite if force_update or if skipped check above passed)
          arrow::write_parquet(points_df, parquet_file_path)
          conversion_successful <- TRUE
          # message(sprintf("Successfully converted %s to %s", file_name, basename(parquet_file_path)))
        } else {
          message(sprintf("Skipping write for %s on %s (No data points after raster_to_points_df)", scientific_name, date_str))
          skipped_count <- skipped_count + 1 # Count as skipped due to no data
        }
      },
      error = function(e) {
        message(sprintf("ERROR converting %s: %s", file_name, e$message))
        error_count <<- error_count + 1 # Use <<- to modify counter in outer scope
      }
    )
    # --- Conversion Logic --- END ---

    # --- Validation and Cleaning Step --- START ---
    if (conversion_successful) {
      validation_result <- validate_and_clean_parquet_file(parquet_file_path)

      if (startsWith(validation_result$status, "removed")) {
        validation_removed_count <- validation_removed_count + 1
        # File was already deleted by the function, just count it
        # We converted it successfully, but it failed validation afterwards
      } else if (validation_result$status == "modified_successfully") {
        validation_modified_count <- validation_modified_count + 1
        processed_count <- processed_count + 1 # Count as processed
      } else if (validation_result$status == "valid") {
        processed_count <- processed_count + 1 # Count as processed
      } else {
        # Handle unexpected status or errors from validation if needed
        message(sprintf("Warning: Unexpected status '%s' from validation for %s", validation_result$status, basename(parquet_file_path)))
        # Decide if this counts as an error? Let's count it as processed for now.
        processed_count <- processed_count + 1
      }
    } else if (fs::file_exists(parquet_file_path)) {
      # If conversion failed BUT the file exists (e.g. from a previous run without force), delete it?
      # Or leave it? Let's leave it for now, relies on force_update logic.
      # message(sprintf("Conversion failed for %s, but parquet file %s exists.", file_name, basename(parquet_file_path)))
    }
    # --- Validation and Cleaning Step --- END ---
  } # End loop through TIFF files

  # Final summary message
  message(sprintf("--- Parquet Conversion & Cleaning Summary for %s: ---", date_str))
  message(sprintf("- TIFFs processed/converted: %d", processed_count + validation_removed_count)) # Includes files later removed by validation
  message(sprintf("- Files standardized/modified by cleaning: %d", validation_modified_count))
  message(sprintf("- Files removed by cleaning (invalid/error): %d", validation_removed_count))
  message(sprintf("- Final valid Parquet files produced/kept: %d", processed_count))
  message(sprintf("- Conversions skipped (Parquet exists or no data): %d", skipped_count))
  message(sprintf("- Conversion errors: %d", error_count))
  message(sprintf("---------------------------------------------------"))

  return(invisible(NULL))
}

# --- Main Execution Loop ---

total_dates <- length(date_sequence)
overall_start_time <- Sys.time()

# Setup parallel processing if needed (based on benchmark optimization recommendations)
no_cores <- parallel::detectCores() - 1
if (no_cores < 1) no_cores <- 1
message(sprintf("Detected %d cores, using %d for parallel processing", parallel::detectCores(), no_cores))

# Create parallel cluster if more than one core is available
if (no_cores > 1) {
  parallel_cluster <- parallel::makeCluster(no_cores)
  message("Parallel processing enabled for raster transformations")
  on.exit(
    {
      message("Stopping parallel cluster")
      parallel::stopCluster(parallel_cluster)
    },
    add = TRUE
  )
} else {
  parallel_cluster <- NULL
  message("Running in single-core mode")
}

message(sprintf("\n=== Starting Update Process for %d Date(s) ===\n", total_dates))

# Fetch the complete key list ONCE before the loop
all_s3_keys <- fetch_all_keys()

if (is.null(all_s3_keys)) {
  stop("Failed to retrieve the complete key list from the S3 bucket. Exiting.")
}

for (i in seq_along(date_sequence)) {
  current_target_date <- date_sequence[i]
  date_str <- format(current_target_date, "%Y-%m-%d")
  message(sprintf("\n=== Processing Date %d/%d: %s ===\n", i, total_dates, date_str))
  date_start_time <- Sys.time()

  # Step 1: Fetch and cache TIFFs using the pre-fetched key list
  message("Step 1: Fetching and Caching TIFF files...")
  fetch_and_cache_tiffs(current_target_date, all_s3_keys, force_download = opts$force)

  # Step 2: Convert downloaded TIFFs to Parquet
  message("\nStep 2: Converting TIFFs to Parquet...")
  convert_tiffs_to_parquet(current_target_date, force_update = opts$force)

  date_end_time <- Sys.time()
  date_duration <- round(as.numeric(difftime(date_end_time, date_start_time, units = "secs")), 2)
  message(sprintf("=== Finished Processing Date %s (Duration: %.2f seconds) ===", date_str, date_duration))
}

# Stop parallel cluster if it was created
if (!is.null(parallel_cluster)) {
  message("\nStopping parallel cluster")
  parallel::stopCluster(parallel_cluster)
}

overall_end_time <- Sys.time()
message(sprintf(
  "\n=== Update Process Completed (Total Duration: %.2f minutes) ===",
  difftime(overall_end_time, overall_start_time, units = "mins")
))

# Optional: Add step for updating bird_info.json if needed here
# message("\nStep 3: Updating bird info JSON...")

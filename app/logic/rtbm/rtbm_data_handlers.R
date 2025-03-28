# /app/view/rtbm/rtbm_data_handlers.R

box::use(
  httr2[
    request, req_url_path_append, req_perform, resp_body_json,
    req_retry, resp_status, resp_body_string
  ],
  jsonlite[read_json, fromJSON, toJSON],
  terra[rast, as.data.frame],
  dplyr[filter, pull, rename, select, distinct, arrange, mutate, bind_rows],
  lubridate[ymd, Date, as_date],
  fs[dir_create, dir_exists, dir_ls, file_exists, path_ext_set, path_file],
  config[get],
  purrr[map, possibly, safely, discard, keep, map_chr],
  stringr[str_extract, str_detect, str_replace, str_extract_all],
  arrow[read_parquet, write_parquet],
  tibble[as_tibble],
  logger[log_info, log_debug, log_error],
  sf
)

# --- Configuration & Setup ---

BASE_DATA_PATH <- get("data_path")
RTBM_DATA_PATH <- file.path(BASE_DATA_PATH, "rtbm")
RTBM_CACHE_PATH <- file.path(RTBM_DATA_PATH, "cache")
RTBM_PARQUET_PATH <- file.path(RTBM_DATA_PATH, "parquet")
RTBM_TEMP_PATH <- file.path(RTBM_DATA_PATH, "temp")
LOCAL_BIRD_INFO_PATH <- file.path(RTBM_DATA_PATH, "bird_info.json")

# Create directories if they don't exist
if (!dir_exists(RTBM_CACHE_PATH)) dir_create(RTBM_CACHE_PATH, recurse = TRUE)
if (!dir_exists(RTBM_PARQUET_PATH)) dir_create(RTBM_PARQUET_PATH, recurse = TRUE)
if (!dir_exists(RTBM_TEMP_PATH)) dir_create(RTBM_TEMP_PATH, recurse = TRUE)

BIRD_INFO_URL <- "https://bird-photos.a3s.fi/bird_info.json"
TIFF_BUCKET_URL <- "https://2007581-webportal.a3s.fi/"

# --- Data Fetching & Caching ---

#' Load Bird Species Information from JSON URL
#' @return A dataframe with bird species info, or NULL on error.
#' @export
load_bird_species_info <- function() {
  # Try to load from local file first (more reliable and faster)
  if (file_exists(LOCAL_BIRD_INFO_PATH)) {
    safe_read_local <- safely(\(file_path) {
      # Read the JSON file using appropriate method
      bird_info <- read_json(file_path)

      if (length(bird_info) == 0) {
        stop("Empty JSON from local file")
      }

      # Process the nested JSON structure
      bird_df <- do.call(rbind, lapply(names(bird_info), function(species_key) {
        info <- bird_info[[species_key]]

        # Check if this is the expected structure
        if (!all(c("common_name", "scientific_name") %in% names(info))) {
          warning("Missing required fields for species: ", species_key)
          return(NULL)
        }

        data.frame(
          common_name = info$common_name,
          scientific_name = info$scientific_name,
          finnish_name = info$finnish_name,
          photo_url = info$photo_url,
          wiki_link = info$wiki_link,
          song_url = info$song_url,
          stringsAsFactors = FALSE
        )
      }))

      # Convert to tibble and arrange by common name
      result <- as_tibble(bird_df) |>
        arrange(common_name) |>
        mutate(
          scientific_name = str_replace(
            scientific_name,
            pattern = " ",
            replacement = "_"
          )
        )

      return(result)
    })

    local_result <- safe_read_local(LOCAL_BIRD_INFO_PATH)
    if (is.null(local_result$error)) {
      message("Loaded bird species info from local file")
      return(local_result$result)
    } else {
      message("Error loading from local file: ", local_result$error, ". Falling back to URL.")
    }
  }

  # Fallback to URL if local file doesn't exist or has issues
  safe_fetch <- safely(\(url) {
    req <- request(url) |> req_retry(max_tries = 3)
    resp <- req_perform(req)
    if (resp_status(resp) >= 300) {
      stop("Failed to fetch bird info. Status: ", resp_status(resp))
    }

    # Get the JSON data
    bird_info <- resp_body_json(resp)

    # Check if it's a nested structure
    if (length(bird_info) == 0) {
      stop("Empty JSON response")
    }

    # Process the nested JSON structure as in the original code
    bird_df <- do.call(rbind, lapply(names(bird_info), function(species_key) {
      info <- bird_info[[species_key]]

      # Check if this is the expected structure
      if (!all(c("common_name", "scientific_name") %in% names(info))) {
        warning("Missing required fields for species: ", species_key)
        return(NULL)
      }

      data.frame(
        common_name = info$common_name,
        scientific_name = info$scientific_name,
        finnish_name = info$finnish_name,
        photo_url = info$photo_url,
        wiki_link = info$wiki_link,
        song_url = info$song_url,
        stringsAsFactors = FALSE
      )
    }))

    # Convert to tibble and arrange by common name
    result <- as_tibble(bird_df) |>
      arrange(common_name) |>
      mutate(
        scientific_name = str_replace(
          scientific_name,
          pattern = " ",
          replacement = "_"
        )
      )

    return(result)
  })

  result <- safe_fetch(BIRD_INFO_URL)
  if (!is.null(result$error)) {
    message("Error loading bird species info from URL: ", result$error)
    return(NULL)
  }
  return(result$result)
}

#' List TIFF files from the data bucket URL
#' Assumes the URL points to an HTML index page. Adapt if listing mechanism differs.
#' @param bucket_url The URL of the data bucket index.
#' @return A character vector of TIFF filenames, or NULL on error.
list_tiff_files_from_bucket <- function(bucket_url) {
  safe_list <- safely(\(url) {
    req <- request(url) |> req_retry(max_tries = 3)
    resp <- req_perform(req)
    if (resp_status(resp) >= 300) {
      stop("Failed to access TIFF bucket index. Status: ", resp_status(resp))
    }
    # Scrape links assuming an HTML index page
    raw_html <- resp_body_string(resp)
    # Extract filenames with tif/tiff extensions using regex pattern
    tiff_files <- str_extract_all(
      raw_html,
      pattern = "href=['\"]([^'\"]*\\.tiff?)['\"]"
    )[[1]] |>
      str_replace("href=['\"]", "") |>
      str_replace("['\"]$", "") |>
      unlist() |>
      unique()

    if (length(tiff_files) == 0) {
      warning("No TIFF files found linked on the bucket index page.")
      return(character(0))
    }
    return(tiff_files)
  })
  result <- safe_list(bucket_url)
  if (!is.null(result$error)) {
    message("Error listing TIFF files from bucket: ", result$error)
    return(NULL)
  }
  return(result$result)
}

#' Fetch and Cache TIFF Files
#' @param force_update Logical, whether to re-download even if files exist. Defaults to FALSE.
#' @return Invisible NULL. Messages indicate progress or errors.
#' @export
fetch_and_cache_tiffs <- function(force_update = FALSE) {
  target_files <- list_tiff_files_from_bucket(TIFF_BUCKET_URL)
  if (is.null(target_files)) {
    message("Could not retrieve list of TIFF files. Skipping download.")
    return(invisible(NULL))
  }
  if (length(target_files) == 0) {
    message("No TIFF files listed in the bucket. Nothing to download.")
    return(invisible(NULL))
  }
  message("Found ", length(target_files), " TIFF files to potentially download.")
  download_results <- map(target_files, \(file_name) {
    file_url <- paste0(TIFF_BUCKET_URL, file_name)
    cache_file_path <- file.path(RTBM_CACHE_PATH, file_name)
    if (file_exists(cache_file_path) && !force_update) {
      return(list(file = file_name, status = "skipped", path = cache_file_path))
    }
    safe_download <- safely(\(url, path_arg) {
      req <- request(url) |> req_retry(max_tries = 3)
      req_perform(req, path = path_arg)
      return(list(file = file_name, status = "downloaded", path = path_arg))
    })
    message("Downloading: ", file_name)
    result <- safe_download(file_url, cache_file_path)
    if (!is.null(result$error)) {
      message("Error downloading ", file_name, ": ", result$error)
      return(list(file = file_name, status = "error", error = result$error))
    }
    return(result$result)
  })
  status_summary <- table(map_chr(download_results, "status"))
  message("TIFF Download Summary:")
  for (status_name in names(status_summary)) {
    message("- ", status_name, ": ", status_summary[[status_name]])
  }
  invisible(NULL)
}

# --- Data Conversion ---

#' Convert Cached TIFFs to Parquet
#' @param force_update Logical, whether to reconvert even if Parquet exists. Defaults to FALSE.
#' @return Invisible NULL. Messages indicate progress or errors.
#' @export
convert_tiffs_to_parquet <- function(force_update = FALSE) {
  tiff_files <- dir_ls(RTBM_CACHE_PATH, regexp = "\\.tiff?$", ignore.case = TRUE)
  if (length(tiff_files) == 0) {
    message("No TIFF files found in cache directory: ", RTBM_CACHE_PATH)
    return(invisible(NULL))
  }
  message("Found ", length(tiff_files), " TIFF files to potentially convert.")
  conversion_results <- map(tiff_files, \(tiff_path) {
    parquet_file_name <- path_ext_set(path_file(tiff_path), ".parquet")
    parquet_path <- file.path(RTBM_PARQUET_PATH, parquet_file_name)
    if (file_exists(parquet_path) && !force_update) {
      return(list(file = path_file(tiff_path), status = "skipped", path = parquet_path))
    }
    safe_convert <- safely(\(tif_in, pq_out) {
      message("Converting: ", path_file(tif_in), " to Parquet.")
      rast_data <- rast(tif_in)

      # Get non-NA values and their coordinates
      vals <- values(rast_data)
      valid_cells <- which(!is.na(vals))

      if (length(valid_cells) == 0) {
        message("No valid values found in raster: ", path_file(tif_in))
        return(list(file = path_file(tif_in), status = "no_data", path = pq_out))
      }

      # Extract coordinates for valid cells
      coords <- xyFromCell(rast_data, valid_cells)

      # Create dataframe with valid values
      df_data <- data.frame(
        x = coords[, 1],
        y = coords[, 2],
        value = vals[valid_cells],
        stringsAsFactors = FALSE
      )

      # Add any necessary cleaning or transformation here
      safe_write <- safely(\(df_data, pq_out) {
        write_parquet(df_data, sink = pq_out)
      })
      result <- safe_write(df_data, pq_out)
      if (!is.null(result$error)) {
        stop("Error writing to Parquet: ", result$error)
      }
      return(list(file = path_file(tif_in), status = "converted", path = pq_out))
    })
    result <- safe_convert(tiff_path, parquet_path)
    if (!is.null(result$error)) {
      message("Error converting ", path_file(tiff_path), ": ", result$error)
      return(list(file = path_file(tiff_path), status = "error", error = result$error))
    }
    return(result$result)
  })
  status_summary <- table(map_chr(conversion_results, "status"))
  message("TIFF to Parquet Conversion Summary:")
  for (status_name in names(status_summary)) {
    message("- ", status_name, ": ", status_summary[[status_name]])
  }
  invisible(NULL)
}

#' Convert raster to points dataframe
#'
#' @param r Raster object
#' @param date Date for the data
#' @return Data frame with x, y coordinates, value, and date
#' @export
raster_to_points_df <- function(r, date = NULL) {
  log_debug("Converting raster to points dataframe for storage")

  tryCatch(
    {
      # Get values and filter out NAs and zeros
      vals <- terra::values(r)
      valid_cells <- which(!is.na(vals) & vals > 0)

      log_debug(paste(
        "Value analysis:",
        "\n  Total cells:", length(vals),
        "\n  Valid cells (non-NA and positive):", length(valid_cells)
      ))

      if (length(valid_cells) == 0) {
        log_debug("No valid (positive) values found in raster")
        return(data.frame())
      }

      # Extract coordinates for valid cells
      coords <- terra::xyFromCell(r, valid_cells)

      # Create dataframe with valid points
      df <- data.frame(
        x = coords[, 1],
        y = coords[, 2],
        value = vals[valid_cells]
      )

      log_debug(paste("Created dataframe with", nrow(df), "points"))
      log_debug(paste("Value range:", min(df$value), "to", max(df$value)))

      # Add date column if provided
      if (!is.null(date)) {
        df$date <- date
      }

      return(df)
    },
    error = function(e) {
      log_error(paste("Error in raster_to_points_df:", e$message))
      return(data.frame())
    }
  )
}

#' Convert points data frame to a raster
#'
#' @param points_df Data frame with x, y coordinates and value
#' @return A raster object
#' @export
points_to_raster <- function(points_df) {
  # Debug information
  print("=============================================")
  print("Starting points_to_raster conversion")
  print(paste("Input data frame has", nrow(points_df), "rows"))
  print(paste("x range:", min(points_df$x), "to", max(points_df$x)))
  print(paste("y range:", min(points_df$y), "to", max(points_df$y)))
  print(paste("Value range:", min(points_df$value, na.rm = TRUE), "to", max(points_df$value, na.rm = TRUE)))

  tryCatch(
    {
      # Create an empty raster with appropriate extent
      ext <- extent(
        min(points_df$x) - 0.05,
        max(points_df$x) + 0.05,
        min(points_df$y) - 0.05,
        max(points_df$y) + 0.05
      )

      print(paste(
        "Creating raster with extent:",
        paste(
          min(points_df$x) - 0.05, max(points_df$x) + 0.05,
          min(points_df$y) - 0.05, max(points_df$y) + 0.05
        )
      ))

      # Create raster with 0.05 degree resolution
      r <- rast(ext, resolution = 0.05)
      print(paste("Empty raster dimensions:", ncol(r), "x", nrow(r)))

      # Set coordinate reference system to WGS84 (EPSG:4326)
      crs(r) <- "EPSG:4326"

      # Convert points_df to sf object for rasterization
      points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = 4326)

      # Rasterize points
      print("Rasterizing points...")
      r <- rasterize(points_sf, r, field = "value", fun = max)

      # Check result
      vals <- values(r)
      non_na_count <- sum(!is.na(vals))
      print(paste("Final raster contains", non_na_count, "non-NA cells"))

      # Validate that we have data in the raster
      if (non_na_count == 0) {
        print("WARNING: Raster has no valid values!")
        return(NULL)
      }

      return(r)
    },
    error = function(e) {
      print(paste("ERROR in points_to_raster:", e$message))
      return(NULL)
    }
  )
}

# --- Data Access & Processing ---

#' Load Parquet Data for a Specific Date Range and Species
#' @param scientific_name The scientific name of the bird species
#' @param start_date Start date of the range (Date object or string in 'YYYY-MM-DD' format)
#' @param end_date End date of the range (Date object or string in 'YYYY-MM-DD' format)
#' @return A list with data and dates, or NULL if files missing/error
#' @export
load_parquet_data <- function(scientific_name, start_date, end_date) {
  start_date <- tryCatch(as_date(start_date), error = function(e) NULL)
  end_date <- tryCatch(as_date(end_date), error = function(e) NULL)

  if (is.null(start_date) || is.null(end_date)) {
    message("Invalid date range provided.")
    return(NULL)
  }

  # Format species directory name (matches actual directory structure)
  # Replace spaces with underscores in the scientific name if needed
  species_name <- str_replace(scientific_name, " ", "_")
  species_dir <- file.path(RTBM_PARQUET_PATH, paste0("species=", species_name))

  if (!dir_exists(species_dir)) {
    message("No data available for species: ", scientific_name)
    return(NULL)
  }

  # Get all dates within the requested range
  all_dates <- seq(from = start_date, to = end_date, by = "day")

  # Load data for each date in the range
  all_data <- list()
  available_dates <- c()

  for (date in all_dates) {
    # Format the date filename according to actual structure (date=YYYY-MM-DD.parquet)
    date_str <- as.character(as.Date(date))
    filename <- paste0("date=", date_str, ".parquet")
    date_file <- file.path(species_dir, filename)

    if (!file_exists(date_file)) {
      # Skip dates with no data
      next
    }

    # Read the parquet file
    safe_read <- safely(read_parquet)
    result <- safe_read(date_file)

    if (!is.null(result$error)) {
      message("Error reading parquet file for ", date_str, ": ", result$error)
      next
    }

    # Add to our data collection
    all_data[[length(all_data) + 1]] <- result$result
    available_dates <- c(available_dates, date)
  }

  if (length(all_data) == 0) {
    message(
      "No data found for species ", scientific_name, " in date range ",
      format(start_date, "%Y-%m-%d"), " to ", format(end_date, "%Y-%m-%d")
    )
    return(NULL)
  }

  # Combine all data frames
  combined_data <- bind_rows(all_data)

  # Return both the data and the list of dates
  return(list(
    data = combined_data,
    dates = available_dates
  ))
}

#' Get Available Dates from Parquet Files
#' @param scientific_name Optional scientific name to filter by. If NULL, returns all available dates.
#' @return A sorted vector of Date objects, or empty vector if none found/parsed.
#' @export
get_available_parquet_dates <- function(scientific_name = NULL) {
  dates <- c()

  if (!is.null(scientific_name)) {
    # If scientific name is provided, only look in that species directory
    species_name <- str_replace(scientific_name, " ", "_")
    species_dir <- file.path(RTBM_PARQUET_PATH, paste0("species=", species_name))

    if (!dir_exists(species_dir)) {
      message("No data directory found for species: ", scientific_name)
      return(as_date(character(0)))
    }

    # Get all date files in the species directory
    date_files <- dir_ls(species_dir, regexp = "\\.parquet$", ignore.case = TRUE)

    # Extract dates from filenames (format is "date=YYYY-MM-DD.parquet")
    dates <- date_files |>
      path_file() |>
      str_extract("\\d{4}-\\d{2}-\\d{2}") |>
      discard(is.na) |>
      as_date(quiet = TRUE) |>
      sort() |>
      unique()
  } else {
    # If no scientific name is provided, scan all species directories
    species_dirs <- dir_ls(RTBM_PARQUET_PATH, type = "directory")

    # For each species directory, extract available dates
    all_dates <- c()
    for (species_dir in species_dirs) {
      date_files <- dir_ls(species_dir, regexp = "\\.parquet$", ignore.case = TRUE)

      species_dates <- date_files |>
        path_file() |>
        str_extract("\\d{4}-\\d{2}-\\d{2}") |>
        discard(is.na) |>
        as_date(quiet = TRUE) |>
        sort()

      all_dates <- c(all_dates, species_dates)
    }

    # Get unique dates across all species
    dates <- all_dates |>
      sort() |>
      unique()
  }

  return(dates)
}

#' Get Photo URL for a Specific Species
#' @param species_info_data Dataframe returned by load_bird_species_info.
#' @param target_common_name The common name of the bird.
#' @return Character string URL or NULL if not found.
#' @export
get_species_photo_url <- function(species_info_data, target_common_name) {
  if (is.null(species_info_data) || !is.data.frame(species_info_data) ||
    !"common_name" %in% names(species_info_data) ||
    !"photo_url" %in% names(species_info_data)) { # Assuming 'photo_url' is the column name
    warning("Invalid species_info_data provided.")
    return(NULL)
  }
  if (is.null(target_common_name) || length(target_common_name) != 1 || !is.character(target_common_name)) {
    warning("Invalid target_common_name provided.")
    return(NULL)
  }
  url_data <- species_info_data |>
    filter(common_name == target_common_name) |>
    pull(photo_url) # Ensure 'photo_url' is correct column name
  if (length(url_data) == 0 || is.na(url_data[[1]]) || url_data[[1]] == "") {
    return(NULL)
  }
  return(url_data[[1]])
}

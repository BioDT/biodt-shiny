# /app/logic/rtbm/rtbm_data_handlers.R

box::use(
  httr2[request, req_perform, resp_body_json, req_retry, resp_status, resp_body_string, resp_status_desc],
  jsonlite[read_json, write_json, fromJSON],
  dplyr[arrange, mutate, filter, bind_rows, rename, group_by, summarise, across, everything, left_join],
  lubridate[as_date, today, days, ymd],
  fs[dir_create, dir_exists, dir_ls, file_exists, path_file],
  config[get],
  purrr[safely, map, discard, map_dfr, possibly, list_rbind, imap, map2_dfr],
  stringr[str_replace, str_extract, str_replace_all],
  arrow[read_parquet],
  tibble[as_tibble, tibble],
  tidyr[pivot_wider, unnest],
  glue[glue],
  sf[st_bbox, st_crs, st_as_sfc, st_read],
)

# --- Configuration & Setup ---

base_data_path <- get("data_path")
rtbm_data_path <- file.path(base_data_path, "rtbm")
rtbm_parquet_path <- file.path(rtbm_data_path, "parquet")

# Ensure directories exist
# if (!dir_exists(rtbm_parquet_path)) {
#   stop(
#     "RTBM Parquet directory not found. Please configure the data_path in your config file.",
#     call. = FALSE
#   )
# }

bird_info_url <- "https://bird-photos.a3s.fi/bird_info.json"
s3_endpoint_base <- "https://2007581-webportal.a3s.fi"

# --- Data Loading Functions ---

#' Load Finland Border GeoJSON Data
#'
#' Loads the Finland border GeoJSON file from the configured data path.
#'
#' @return An sf object representing the Finland border, or NULL if an error occurs or the file is not found.
#' @export
load_finland_border_geojson <- function() {
  tryCatch(
    {
      # Construct the full path to the GeoJSON file
      border_path <- file.path(get("data_path"), "rtbm", "finland_border.geojson")

      # Check if the file exists
      if (!file.exists(border_path)) {
        message("Finland border GeoJSON not found at: ", border_path)
        return(NULL)
      }

      # Read the GeoJSON file
      sf_object <- st_read(border_path, quiet = TRUE)
      message("Successfully loaded Finland border GeoJSON from: ", border_path)
      return(sf_object)
    },
    error = function(e) {
      message("Error loading Finland border GeoJSON: ", e$message)
      return(NULL)
    }
  )
}

#' Load bird species information from URL
#'
#' Fetches bird info from a predefined URL.
#'
#' @return A tibble containing bird species information (common name, scientific name, photo URL, etc.)
#'         sorted by common name, or NULL if loading fails.
#' @export
load_bird_species_info <- function() {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

  # --- Helper function to process raw JSON data into a tibble ---
  process_bird_info_json <- function(bird_info_raw) {
    if (is.null(bird_info_raw) || length(bird_info_raw) == 0) {
      stop("Raw bird info JSON is empty or NULL.")
    }

    bird_df <- imap(bird_info_raw, \(info, name_key) {
      if (is.null(info$scientific_name) || info$scientific_name == "") {
        return(NULL)
      }
      data.frame(
        joinKeyScientificName = name_key %||% NA_character_,
        displayScientificName = info$scientific_name %||% NA_character_,
        commonName = info$common_name %||% NA_character_,
        finnishName = info$finnish_name %||% NA_character_,
        photoUrl = info$photo_url %||% NA_character_,
        wikiLink = info$wiki_link %||% NA_character_,
        songUrl = info$song_url %||% NA_character_,
        speciesUrl = info$species_url %||% NA_character_,
        stringsAsFactors = FALSE
      )
    }) |>
      discard(\(x) is.null(x) || is.na(x$displayScientificName))

    if (length(bird_df) == 0) {
      stop("No valid bird entries found after processing JSON.")
    }

    result <- bind_rows(bird_df) |>
      rename(
        join_key_scientific_name = joinKeyScientificName,
        display_scientific_name = displayScientificName,
        common_name = commonName,
        photo_url = photoUrl,
        wiki_link = wikiLink,
        song_url = songUrl,
        species_url = speciesUrl
      ) |>
      arrange(common_name) |>
      mutate(
        join_key_scientific_name = str_replace_all(
          join_key_scientific_name,
          pattern = " ",
          replacement = "_"
        ),
        display_scientific_name = str_replace_all(
          display_scientific_name,
          pattern = " ",
          replacement = "_"
        )
      )
    return(result)
  }

  # 1. Try loading from local file first
  local_file_path <- file.path(rtbm_data_path, "bird_info.json")

  if (file.exists(local_file_path)) {
    message("Attempting to load bird species info from local file: ", local_file_path)
    safe_read_local <- safely(\(path) {
      raw_data <- read_json(path, simplifyVector = FALSE)
      process_bird_info_json(raw_data)
    })
    local_result <- safe_read_local(local_file_path)

    if (is.null(local_result$error) && !is.null(local_result$result)) {
      message("Successfully loaded bird species info from local file.")
      return(local_result$result)
    } else {
      warning(
        "Failed to load or parse local bird_info.json: ",
        local_result$error$message,
        ". Falling back to URL."
      )
    }
  } else {
    message("Local bird_info.json not found at: ", local_file_path, ". Attempting to fetch from URL.")
  }

  # 2. Fetch from URL if local loading failed or file not found
  message("Attempting to fetch bird species info from URL: ", bird_info_url)
  safe_fetch_url <- safely(\(url) {
    req <- request(url) |> req_retry(max_tries = 3)
    resp <- req_perform(req)

    if (resp_status(resp) >= 300) {
      stop(paste("Failed to fetch bird info from URL. Status:", resp_status(resp), resp_status_desc(resp)))
    }
    bird_info_raw <- resp_body_json(resp) # httr2 default is simplifyVector = FALSE
    process_bird_info_json(bird_info_raw)
  })

  url_result <- safe_fetch_url(bird_info_url)

  if (!is.null(url_result$error)) {
    final_error_msg <- paste(
      "Failed to load bird species info from both local file and URL. URL error: ",
      url_result$error$message
    )
    warning(final_error_msg)
    stop(final_error_msg)
  } else if (!is.null(url_result$result)) {
    message("Successfully loaded bird species info from URL.")
    # Optionally, save the fetched data to the local file for future use
    tryCatch(
      {
        write_json(
          resp_body_json(safe_fetch_url(bird_info_url)$result),
          local_file_path,
          auto_unbox = TRUE,
          pretty = TRUE
        )
        message("Saved fetched bird_info.json to local cache: ", local_file_path)
      },
      error = function(e_save) {
        warning("Could not save bird_info.json to local cache: ", e_save$message)
      }
    )
    return(url_result$result)
  }

  # Fallback stop, though one of the above should have returned or raised a more specific error
  stop("Failed to load bird information after attempting all sources.")
}

#' Load Parquet data for a specific species and date range
#'
#' Loads Parquet files for a given bird species corresponding to dates
#' within the specified start and end dates.
#'
#' @param scientific_name The scientific name of the bird species (e.g., "Parus major").
#' @param start_date The start date for filtering data (YYYY-MM-DD or Date object).
#' @param end_date The end date for filtering data (YYYY-MM-DD or Date object).
#'
#' @return A list containing two elements:
#'         `data`: A tibble combining data from the relevant Parquet files, or NULL if no data is found.
#'         `dates`: A vector of Date objects representing the dates for which data was successfully loaded.
#' @export
load_parquet_data <- function(scientific_name, start_date, end_date) {
  # Input validation
  if (missing(scientific_name) || is.null(scientific_name) || scientific_name == "") {
    stop("Scientific name must be provided.")
  }
  if (missing(start_date) || missing(end_date)) {
    stop("Both start_date and end_date must be provided.")
  }

  # Ensure dates are Date objects
  start_date <- tryCatch(as_date(start_date), warning = function(w) NA, error = function(e) NA)
  end_date <- tryCatch(as_date(end_date), warning = function(w) NA, error = function(e) NA)

  if (is.na(start_date) || is.na(end_date)) {
    stop("Invalid date format provided. Please use YYYY-MM-DD or Date objects.")
  }
  if (start_date > end_date) {
    stop("start_date cannot be after end_date.")
  }

  # Construct the path
  species_path_name <- str_replace(scientific_name, " ", "_")
  species_dir <- file.path(rtbm_parquet_path, paste0("species=", species_path_name))

  if (!dir_exists(species_dir)) {
    message("Data directory not found for species: ", scientific_name, " at ", species_dir)
    return(list(data = NULL, dates = as.Date(character(0))))
  }

  # List files
  all_files <- dir_ls(species_dir, regexp = "\\.parquet$", ignore.case = TRUE)

  if (length(all_files) == 0) {
    message("No Parquet files found in directory: ", species_dir)
    return(list(data = NULL, dates = as.Date(character(0))))
  }

  # Extract dates and filter
  file_dates <- all_files |>
    path_file() |>
    str_extract("(?<=date=)\\d{4}-\\d{2}-\\d{2}(?=\\.parquet)") |>
    as_date()

  file_info <- tibble(path = all_files, date = file_dates) |>
    filter(!is.na(date), date >= start_date, date <= end_date)

  if (nrow(file_info) == 0) {
    message(
      "No Parquet files found for species '",
      scientific_name,
      "' between ",
      start_date,
      " and ",
      end_date
    )
    return(list(data = NULL, dates = as.Date(character(0))))
  }

  # Read data
  safe_read_parquet <- safely(read_parquet)
  data_list <- map(file_info$path, \(p) {
    res <- safe_read_parquet(p)
    if (!is.null(res$error)) {
      warning("Error reading parquet file '", p, "': ", res$error)
      return(NULL) # Return NULL for failed reads
    }
    # Add date column from filename if not present in data
    if (!"date" %in% names(res$result)) {
      file_date <- str_extract(path_file(p), "(?<=date=)\\d{4}-\\d{2}-\\d{2}")
      res$result <- mutate(res$result, date = as_date(file_date))
    }
    return(res$result)
  })

  # Filter out NULLs (failed reads) and get successful dates
  successful_indices <- !sapply(data_list, is.null)
  valid_data <- data_list[successful_indices]
  successful_dates <- file_info$date[successful_indices]

  if (length(valid_data) == 0) {
    message("Failed to read any valid Parquet files for the specified criteria.")
    return(list(data = NULL, dates = as.Date(character(0))))
  }

  # Combine data
  combined_data <- bind_rows(valid_data)

  message(
    "Successfully loaded data for ",
    length(successful_dates),
    " dates for species '",
    scientific_name,
    "' between ",
    start_date,
    " and ",
    end_date
  )

  # Create a named vector of data paths for each date (without setNames)
  data_paths <- as.character(file_info$path[successful_indices])
  names(data_paths) <- as.character(successful_dates)

  return(list(data = combined_data, dates = successful_dates, data_paths = data_paths))
}

# Create a simple bounding box for Finland as fallback (Internal helper)
create_finland_bbox <- function() {
  # Create a bounding box for Finland as a fallback
  bbox <- st_bbox(c(xmin = 19, xmax = 32, ymin = 59, ymax = 71), crs = st_crs(4326))
  bbox_sf <- st_as_sfc(bbox)
  return(bbox_sf)
}

#' Preload Bird Summary Data from Web Portal
#'
#' Fetches daily bird observation summary data from the A3S web portal for a
#' given date range, aggregates counts per species per day, and returns a
#' summarized data frame.
#'
#' @param start_date Date object or string (YYYY-MM-DD). The start date for fetching data.
#'                   Defaults to "2025-01-16".
#' @param end_date Date object or string (YYYY-MM-DD). The end date for fetching data.
#'                 Defaults to yesterday's date.
#' @param progress_callback Optional function to call for progress updates. Should accept
#'                         parameters: current (integer), total (integer), message (string).
#' @return A tibble where the first column is `date` and subsequent columns
#'         represent bird species, containing the total daily observation count.
#'         Returns NULL if no data can be fetched or processed.
#'         Example format:
#'         | date       | Bird A | Bird B | ... |
#'         |------------|-------|-------|-----|
#'         | 2025-05-01 | 100   | 200   | ... |
#'         | 2025-05-02 | 120   | 180   | ... |
#' @export
preload_summary_data <- function(start_date = "2025-01-16", end_date = NULL, progress_callback = NULL) {
  # --- Input Validation and Date Setup ---
  start_date <- tryCatch(ymd(start_date), error = function(e) stop("Invalid start_date format. Use YYYY-MM-DD."))

  if (is.null(end_date)) {
    end_date <- today() - days(1)
  } else {
    end_date <- tryCatch(ymd(end_date), error = function(e) stop("Invalid end_date format. Use YYYY-MM-DD."))
  }

  if (start_date > end_date) {
    stop("start_date cannot be after end_date.")
  }

  date_sequence <- seq.Date(from = start_date, to = end_date, by = "day")
  total_dates <- length(date_sequence)
  base_url <- "https://2007581-webportal.a3s.fi/daily/"

  # --- Helper function to fetch and process data for a single date ---
  fetch_and_process_date <- function(current_date, index) {
    date_str <- format(current_date, "%Y-%m-%d")
    url <- glue(base_url, "{date_str}/web_portal_summary_data.json")
    
    # Call progress callback if provided
    if (!is.null(progress_callback)) {
      progress_callback(
        current = index,
        total = total_dates,
        message = paste("Fetching", index, "of", total_dates, ":", date_str)
      )
    }
    
    message("Fetching data for: ", date_str, " from ", url)

    # Safely perform request and parse JSON
    safe_req_perform <- possibly(req_perform, otherwise = NULL)
    safe_from_json <- possibly(fromJSON, otherwise = NULL)

    resp <- request(url) |>
      req_retry(max_tries = 2, is_transient = \(resp) resp_status(resp) %in% c(429, 500, 503)) |>
      safe_req_perform()

    # Check if request failed (possibly returned NULL) or status is not 200 OK
    if (is.null(resp) || resp_status(resp) != 200) {
      status_msg <- if (!is.null(resp)) resp_status_desc(resp) else "Connection error"
      warning(glue("Failed to fetch data for {date_str}: {status_msg}"))
      return(NULL)
    }

    json_data <- resp |>
      resp_body_string() |>
      # Parse directly, assuming top-level object has species names as keys
      safe_from_json(simplifyVector = TRUE) # simplifyVector can help if counts are vectors

    if (is.null(json_data) || length(json_data) == 0) {
      warning(glue("No valid JSON data found or failed to parse for {date_str}"))
      return(NULL)
    }

    # Process the named list structure: { "SpeciesA": [counts], "SpeciesB": [counts], ... }
    daily_summary <- tryCatch(
      {
        species_names <- names(json_data)
        if (is.null(species_names) || length(species_names) == 0) {
          warning(glue("JSON for {date_str} is not a named list or is empty after parsing."))
          return(NULL)
        }

        # Iterate through species names, sum counts, create tibble
        map_dfr(species_names, function(spp_name) {
          counts <- json_data[[spp_name]]
          # Ensure counts is numeric and handle potential NULLs/errors
          if (is.null(counts) || !is.numeric(counts)) {
            warning(glue("Invalid or non-numeric counts for species '{spp_name}' on {date_str}"))
            total_count <- 0 # Or NA depending on desired handling
          } else {
            total_count <- sum(counts, na.rm = TRUE)
          }
          tibble(species = spp_name, total_count = total_count)
        })
      },
      error = function(e) {
        warning(glue("Error processing JSON structure for {date_str}: {e$message}"))
        return(NULL)
      }
    )

    if (is.null(daily_summary) || nrow(daily_summary) == 0) {
      return(NULL)
    }

    # Add date column
    daily_summary$date <- current_date
    return(daily_summary)
  }

  # --- Fetch data for all dates and combine ---
  all_data_long <- map2_dfr(date_sequence, seq_along(date_sequence), fetch_and_process_date)

  # Final progress update
  if (!is.null(progress_callback)) {
    progress_callback(
      current = total_dates,
      total = total_dates,
      message = "Processing data..."
    )
  }

  if (nrow(all_data_long) == 0) {
    warning("No summary data could be fetched or processed for the specified date range.")
    return(NULL)
  }

  # --- Pivot to wide format ---
  summary_data_wide <- all_data_long |>
    pivot_wider(
      names_from = species,
      values_from = total_count,
      values_fill = 0 # Replace missing counts (NA) with 0
    ) |>
    arrange(date) # Ensure chronological order

  message("Successfully preloaded summary data from ", start_date, " to ", end_date)
  return(summary_data_wide)
}

# /app/logic/rtbm/rtbm_data_handlers.R

box::use(
  httr2[request, req_perform, resp_body_json, req_retry, resp_status, resp_body_string, resp_status_desc],
  jsonlite[read_json, write_json, fromJSON],
  dplyr[arrange, mutate, filter, bind_rows, rename, group_by, summarise, across, everything, left_join],
  lubridate[as_date, today, days, ymd],
  fs[dir_create, dir_exists, dir_ls, file_exists, path_file],
  config[get],
  purrr[safely, map, discard, map_dfr, possibly, list_rbind],
  stringr[str_replace, str_extract],
  arrow[read_parquet],
  tibble[as_tibble, tibble],
  tidyr[pivot_wider, unnest],
  glue[glue],
  sf[st_read, st_bbox, st_crs, st_as_sfc] # Added sf functions
)

# --- Configuration & Setup ---

base_data_path <- get("data_path")
rtbm_data_path <- file.path(base_data_path, "rtbm")
rtbm_parquet_path <- file.path(rtbm_data_path, "parquet")
local_bird_info_path <- file.path(rtbm_data_path, "bird_info.json")

# Create directories if they don't exist
if (!dir_exists(rtbm_parquet_path)) dir_create(rtbm_parquet_path, recurse = TRUE)

bird_info_url <- "https://bird-photos.a3s.fi/bird_info.json"
s3_endpoint_base <- "https://2007581-webportal.a3s.fi"

# --- Data Loading Functions ---

#' Load bird species information from local file or URL
#'
#' Tries to load bird info from a local JSON file first. If the file doesn't exist
#' or fails to load, it attempts to fetch the data from a predefined URL and
#' saves it locally upon success.
#'
#' @return A tibble containing bird species information (common name, scientific name, photo URL, etc.)
#'         sorted by common name, or NULL if loading fails from both sources.
#' @export
load_bird_species_info <- function() {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

  # Try to load from local file first
  if (file_exists(local_bird_info_path)) {
    safe_read_local <- safely(\(file_path) {
      bird_info <- read_json(file_path)

      if (length(bird_info) == 0) {
        stop("Empty JSON from local file")
      }

      # Process JSON data into a list of data frames
      bird_df <- map(bird_info, \(info) {
        # Basic validation for essential fields
        if (is.null(info$scientific_name) || info$scientific_name == "") {
          return(NULL)
        }
        data.frame(
          scientificName = info$scientific_name %||% NA_character_,
          commonName = info$common_name %||% NA_character_,
          finnishName = info$finnish_name %||% NA_character_,
          photoUrl = info$photo_url %||% NA_character_,
          wikiLink = info$wiki_link %||% NA_character_,
          songUrl = info$song_url %||% NA_character_,
          speciesUrl = info$species_url %||% NA_character_,
          stringsAsFactors = FALSE
        )
      }) |> discard(\(x) is.na(x$scientificName)) # Filter out entries with missing scientificName

      if (length(bird_df) == 0) {
        stop("No valid bird entries found after processing local JSON")
      }

      result <- bind_rows(bird_df) |> # Combine list of tibbles into one
        rename( # Rename columns to snake_case
          scientific_name = scientificName,
          common_name = commonName,
          photo_url = photoUrl,
          wiki_link = wikiLink,
          song_url = songUrl,
          species_url = speciesUrl
        ) |>
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

    local_result <- safe_read_local(local_bird_info_path)
    if (is.null(local_result$error)) {
      message("Loaded bird species info from local file: ", local_bird_info_path)
      return(local_result$result)
    } else {
      warning("Error loading bird info from local file: ", local_result$error, ". Falling back to URL.")
    }
  } else {
    message("Local bird info file not found: ", local_bird_info_path, ". Attempting to fetch from URL.")
  }

  # Fallback to URL
  safe_fetch <- safely(\(url) {
    req <- request(url) |> req_retry(max_tries = 3)
    resp <- req_perform(req)

    if (resp_status(resp) >= 300) {
      stop(paste("Failed to fetch bird info from URL. Status:", resp_status(resp)))
    }

    raw_content <- resp_body_string(resp)
    bird_info <- fromJSON(raw_content)

    if (length(bird_info) == 0) {
      stop("Empty JSON response received from URL")
    }

    # Process JSON data (same logic as local file processing)
    bird_df <- map(bird_info, \(info) {
      if (is.null(info$scientific_name) || info$scientific_name == "") {
        return(NULL)
      }
      data.frame(
        scientificName = info$scientific_name %||% NA_character_,
        commonName = info$common_name %||% NA_character_,
        finnishName = info$finnish_name %||% NA_character_,
        photoUrl = info$photo_url %||% NA_character_,
        wikiLink = info$wiki_link %||% NA_character_,
        songUrl = info$song_url %||% NA_character_,
        speciesUrl = info$species_url %||% NA_character_,
        stringsAsFactors = FALSE
      )
    }) |> discard(\(x) is.na(x$scientificName)) # Filter out entries with missing scientificName

    if (length(bird_df) == 0) {
      stop("No valid bird entries found after processing JSON from URL")
    }

    result <- bind_rows(bird_df) |> # Combine list of tibbles into one
      rename( # Rename columns to snake_case
        scientific_name = scientificName,
        common_name = commonName,
        photo_url = photoUrl,
        wiki_link = wikiLink,
        song_url = songUrl,
        species_url = speciesUrl
      ) |
      arrange(common_name) |
      mutate(
        scientific_name = str_replace(
          scientific_name,
          pattern = " ",
          replacement = "_"
        )
      )

    # Attempt to save the fetched JSON data locally
    tryCatch(
      {
        write_json(bird_info, local_bird_info_path, auto_unbox = TRUE, pretty = TRUE)
        message("Successfully fetched bird info from URL and saved locally to: ", local_bird_info_path)
      },
      error = function(e) {
        warning("Fetched bird info from URL but failed to save locally: ", e$message)
      }
    )

    return(result)
  })

  url_result <- safe_fetch(bird_info_url)
  if (!is.null(url_result$error)) {
    warning("Error loading bird species info from URL: ", url_result$error)
    return(NULL)
  } else if (!is.null(url_result$result)) {
    return(url_result$result)
  }

  # If both methods fail
  stop("Failed to load bird information from both local file and URL.")
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
      "No Parquet files found for species '", scientific_name, "' between ",
      start_date, " and ", end_date
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
    "Successfully loaded data for ", length(successful_dates), " dates for species '",
    scientific_name, "' between ", start_date, " and ", end_date
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

#' Load Finland Border GeoJSON Data
#'
#' Reads the GeoJSON file containing the border of Finland.
#' Handles potential errors if the file is missing or invalid, returning a bounding box as fallback.
#'
#' @param base_path The base path for data files, defaults to constant.
#' @return An sf object representing the Finland border or a fallback bounding box.
#' @export
get_finland_border <- function(base_path = base_data_path) { # Use snake_case constant
  # Construct the full path to the GeoJSON file
  finland_file <- file.path(base_path, "rtbm", "finland_border.geojson")

  if (file_exists(finland_file)) {
    tryCatch(
      {
        # Read border file
        finland <- st_read(finland_file, quiet = TRUE)
        message("Finland border data loaded successfully from: ", finland_file)
        return(finland)
      },
      error = function(e) {
        # If reading fails, create a bounding box
        warning("Error reading Finland border file ('", finland_file, "'): ", e$message, ". Using a bounding box instead.")
        create_finland_bbox()
      }
    )
  } else {
    warning("Finland border file not found ('", finland_file, "'), using a bounding box instead.")
    create_finland_bbox()
  }
}

# --- Web Portal Summary Data ---

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
#' @return A tibble where the first column is `date` and subsequent columns
#'         represent bird species, containing the total daily observation count.
#'         Returns NULL if no data can be fetched or processed.
#'         Example format:
#'         | date       | Bird A | Bird B | ... |
#'         |------------|-------|-------|-----|
#'         | 2025-05-01 | 100   | 200   | ... |
#'         | 2025-05-02 | 120   | 180   | ... |
#' @export
preload_summary_data <- function(start_date = "2025-01-16", end_date = NULL) {
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
  base_url <- "https://2007581-webportal.a3s.fi/daily/"

  # --- Helper function to fetch and process data for a single date ---
  fetch_and_process_date <- function(current_date) {
    date_str <- format(current_date, "%Y-%m-%d")
    url <- glue(base_url, "{date_str}/web_portal_summary_data.json")
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
  all_data_long <- map_dfr(date_sequence, fetch_and_process_date)

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

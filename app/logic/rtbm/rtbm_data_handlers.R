# /app/logic/rtbm/rtbm_data_handlers.R

box::use(
  httr2[request, req_perform, resp_body_json, req_retry, resp_status, resp_body_string],
  jsonlite[read_json, write_json, fromJSON],
  dplyr[arrange, mutate, filter, bind_rows, rename],
  lubridate[as_date],
  fs[dir_create, dir_exists, dir_ls, file_exists, path_file],
  config[get],
  purrr[safely, map, discard],
  stringr[str_replace, str_extract],
  arrow[read_parquet],
  tibble[as_tibble, tibble],
)

# --- Configuration & Setup ---

base_data_path <- get("data_path")
rtbm_data_path <- file.path(base_data_path, "rtbm")
rtbm_parquet_path <- file.path(rtbm_data_path, "parquet")
local_bird_info_path <- file.path(rtbm_data_path, "bird_info.json")

# Create directories if they don't exist
if (!dir_exists(rtbm_parquet_path)) dir_create(rtbm_parquet_path, recurse = TRUE)

bird_info_url <- "https://bird-photos.a3s.fi/bird_info.json"

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

  return(list(data = combined_data, dates = successful_dates))
}

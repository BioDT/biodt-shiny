box::use(
  # Data handling
  arrow[
    read_parquet, write_parquet, Schema, schema,
    float64, string, date32, ParquetFileWriter, ParquetFileReader
  ],
  dplyr[filter, select, mutate, arrange, pull, bind_rows, n],
  sf[st_crs, st_as_sf, st_transform, st_coordinates],
  terra[rast, values, xyFromCell, crs],
  raster[raster, projectRaster, values, xyFromCell, extent, crs, `crs<-`],
  lubridate[today, as_date, days, interval],

  # File system operations
  fs[dir_create, path, path_join],

  # HTTP requests
  httr2[request, req_perform, resp_status, resp_body_json, req_method, req_error],

  # JSON handling
  jsonlite[fromJSON, toJSON],

  # JSON/Data parsing
  tibble[tibble, as_tibble],

  # Logging and utilities
  logger[log_info, log_error, log_success, log_warn],
  memoise[memoise, forget],
  utils[download.file],
)

# Import infix operator separately
`%within%` <- lubridate::`%within%`

#' Initialize storage directories for RTBM data
#'
#' @param base_dir Base directory for RTBM data
#' @return List of path objects for various data directories
#' @noRd
init_storage_dirs <- function(base_dir = "/home/artyinfact/biodt-shiny/app/data/rtbm") {
  # Create main directories
  parquet_dir <- path(base_dir, "parquet")
  cache_dir <- path(base_dir, "cache")

  # Create species-specific directories if they don't exist
  dir_create(parquet_dir, recurse = TRUE, mode = "0755")
  dir_create(cache_dir, recurse = TRUE, mode = "0755")

  return(list(
    base_dir = base_dir,
    parquet_dir = parquet_dir,
    cache_dir = cache_dir
  ))
}

#' Load bird species information from JSON file or remote URL
#'
#' @param use_local Whether to use locally cached data (TRUE) or fetch from remote (FALSE)
#' @param base_dir Base directory for RTBM data
#' @return Tibble containing bird species information
#' @export
load_bird_species <- function(use_local = TRUE,
                              base_dir = "/home/artyinfact/biodt-shiny/app/data/rtbm") {
  local_path <- path(base_dir, "bird_info.json")

  if (use_local && file.exists(local_path)) {
    # Use local file
    bird_info <- fromJSON(local_path)
  } else {
    # Use remote URL
    bird_info_url <- "https://bird-photos.a3s.fi/bird_info.json"
    bird_info <- fromJSON(bird_info_url)

    # Save locally for future use
    write(
      jsonlite::toJSON(bird_info, auto_unbox = TRUE, pretty = TRUE),
      local_path
    )
  }

  # Process the nested JSON structure
  # Convert the list to a dataframe where each row is a bird species
  bird_df <- do.call(rbind, lapply(names(bird_info), function(species_key) {
    info <- bird_info[[species_key]]
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
  bird_info_tbl <- as_tibble(bird_df) |>
    arrange(common_name) |>
    mutate(
      scientific_name = stringr::str_replace(
        string = scientific_name,
        pattern = " ",
        replacement = "_"
      )
    )

  return(bird_info_tbl)
}

#' Get tif URL for a specific date and species
#'
#' @param date Date for the data
#' @param scientific_name Scientific name of the bird species
#' @return URL string for the TIF file
#' @noRd
get_tif_url <- function(date, scientific_name) {
  # Format date for URL
  date_str <- format(as.Date(date), "%Y-%m-%d")

  # Build URL for the tif file
  paste0(
    "https://2007581-webportal.a3s.fi/daily/",
    date_str, "/",
    scientific_name,
    "_occurrences.tif"
  )
}

#' Download and process raster data for a specific date and species
#'
#' @param date Date to fetch data for
#' @param scientific_name Scientific name of the bird species
#' @param cache_dir Directory for caching temporary files
#' @return A list containing the processed data or NULL if not available
#' @noRd
download_and_process_raster <- function(date, scientific_name, cache_dir) {
  # Convert date to character string for URLs and filenames
  date_str <- as.character(as.Date(date))

  # Get URL for the TIF file
  url_tif <- get_tif_url(date, scientific_name)

  # Create path for cache file
  tmp_file <- path(cache_dir, paste0(scientific_name, "_", date_str, ".tif"))

  # Ensure cache directory exists
  dir_create(cache_dir, recurse = TRUE, mode = "0755")

  log_info(paste("Processing data for", scientific_name, "on", date_str))

  # Check if the TIF file already exists locally
  if (file.exists(tmp_file) && file.size(tmp_file) > 0) {
    log_info(paste("Using cached file for", scientific_name, "on", date_str))
  } else {
    # File doesn't exist locally, try to download it
    tryCatch(
      {
        # Check if file exists remotely first
        log_info(paste("Checking if remote file exists:", url_tif))
        resp <- httr2::request(url_tif) |>
          httr2::req_method("HEAD") |>
          httr2::req_error(is_error = function(resp) FALSE) |>
          httr2::req_perform()

        status <- httr2::resp_status(resp)

        if (status != 200) {
          log_info(paste("No data available for", scientific_name, "on", date_str, "- Status:", status))
          return(NULL) # Skip this date if URL is not available
        }

        # Download the file
        log_info(paste("Downloading data for", scientific_name, "on", date_str))
        download_result <- download.file(url_tif, tmp_file, mode = "wb", quiet = TRUE)

        if (download_result != 0 || !file.exists(tmp_file) || file.size(tmp_file) == 0) {
          log_error(paste("Failed to download data for", scientific_name, "on", date_str))
          return(NULL) # Skip if download failed
        }
      },
      error = function(e) {
        log_error(paste("Error downloading data:", e$message))
        return(NULL)
      }
    )
  }

  # Load and process raster if file exists
  if (file.exists(tmp_file) && file.size(tmp_file) > 0) {
    tryCatch(
      {
        # Load raster data
        r <- terra::rast(tmp_file)

        # Reproject if necessary to ensure it works with leaflet
        if (terra::crs(r) != "EPSG:4326") {
          r <- terra::project(r, "EPSG:4326")
        }

        # Check if the raster has valid values
        vals <- terra::values(r)
        valid_vals <- vals[!is.na(vals)]

        if (length(valid_vals) > 0) {
          log_info(paste("Successfully loaded raster with", length(valid_vals), "valid points"))
          return(r)
        } else {
          log_warning(paste("Raster has no valid values for", scientific_name, "on", date_str))
          return(NULL)
        }
      },
      error = function(e) {
        log_error(paste("Error processing raster:", e$message))
        return(NULL)
      }
    )
  } else {
    log_warning(paste("No data file available for", scientific_name, "on", date_str))
    return(NULL)
  }
}

#' Convert raster data to a structured dataframe
#'
#' @param raster_data Raster data from terra package
#' @param date Date of the observation
#' @param scientific_name Scientific name of the species
#' @return A dataframe with processed raster data
#' @noRd
raster_to_dataframe <- function(raster_data, date, scientific_name) {
  if (is.null(raster_data)) {
    return(NULL)
  }

  # Convert to raster for compatibility with existing code
  r_raster <- raster(raster_data)

  # Get values and filter out NAs
  vals <- values(r_raster)
  valid_cells <- which(!is.na(vals))

  if (length(valid_cells) == 0) {
    return(NULL)
  }

  # Extract coordinates for valid cells
  coords <- xyFromCell(r_raster, valid_cells)

  # Create dataframe
  result_df <- tibble(
    date = as.Date(date),
    scientific_name = scientific_name,
    longitude = coords[, 1],
    latitude = coords[, 2],
    intensity = vals[valid_cells]
  )

  return(result_df)
}

#' Process and store data for a species over a date range
#'
#' @param species_name Common name of the species
#' @param scientific_name Scientific name of the species
#' @param start_date Start date for data processing
#' @param end_date End date for data processing
#' @param dirs Directory structure from init_storage_dirs
#' @return TRUE if processing was successful, FALSE otherwise
#' @export
process_species_data <- function(species_name,
                                 scientific_name,
                                 start_date,
                                 end_date,
                                 dirs = init_storage_dirs()) {
  # Create species directory if it doesn't exist
  species_dir <- path(dirs$parquet_dir, paste0("species=", scientific_name))
  dir_create(species_dir, recurse = TRUE, mode = "0755")

  # Process each date - convert to Date objects first, then to character strings
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  dates <- seq(from = start_date, to = end_date, by = "day")

  for (date in dates) {
    # Check if we already have data for this date - convert to character string
    date_str <- as.character(date)
    output_file <- path(species_dir, paste0("date=", date_str, ".parquet"))

    # Skip if file already exists
    if (file.exists(output_file)) {
      log_info(paste("Data for", scientific_name, "on", date_str, "already exists, skipping"))
      next
    }

    # Download and process data
    log_info(paste("Processing", species_name, "(", scientific_name, ")", "for", date_str))
    raster_data <- download_and_process_raster(date, scientific_name, dirs$cache_dir)

    if (is.null(raster_data)) {
      log_info(paste("No valid data for", scientific_name, "on", date_str))
      next
    }

    # Convert to dataframe
    df <- raster_to_dataframe(raster_data, date, scientific_name)

    if (is.null(df) || nrow(df) == 0) {
      log_info(paste("No valid observations for", scientific_name, "on", date_str))
      next
    }

    # Write to parquet file
    write_parquet(df, output_file)
    log_success(paste("Saved", nrow(df), "observations for", scientific_name, "on", date_str))
  }

  return(TRUE)
}

#' Process data for all species for recent dates
#'
#' @param days_back Number of days back to process
#' @param bird_info Bird species information dataframe
#' @param dirs Directory structure from init_storage_dirs
#' @return TRUE if processing was successful, FALSE otherwise
#' @export
process_recent_data <- function(days_back = 14,
                                bird_info = load_bird_species(),
                                dirs = init_storage_dirs()) {
  # Convert dates to character strings to avoid formatting issues
  end_date <- as.Date(today())
  start_date <- as.Date(end_date - days(days_back))
  end_date_str <- as.character(end_date)
  start_date_str <- as.character(start_date)

  log_info(paste("Processing data for all species from", start_date_str, "to", end_date_str))

  results <- list()

  for (i in seq_len(nrow(bird_info))) {
    species_row <- bird_info[i, ]
    results[[i]] <- process_species_data(
      species_name = species_row$common_name,
      scientific_name = species_row$scientific_name,
      start_date = start_date,
      end_date = end_date,
      dirs = dirs
    )
  }

  return(all(unlist(results)))
}

#' Load data for a specific species and date range
#'
#' @param scientific_name Scientific name of the species
#' @param start_date Start date for data loading
#' @param end_date End date for data loading
#' @param dirs Directory structure from init_storage_dirs
#' @return Dataframe with bird observation data
#' @export
load_species_data <- function(scientific_name,
                              start_date,
                              end_date,
                              dirs = init_storage_dirs()) {
  # Create date sequence and convert to character strings
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  dates <- seq(from = start_date, to = end_date, by = "day")
  date_strings <- as.character(dates)

  # Get path to species directory
  species_dir <- path(dirs$parquet_dir, paste0("species=", scientific_name))

  if (!dir.exists(species_dir)) {
    log_warn(paste("No data directory found for", scientific_name))
    return(NULL)
  }

  # Look for files matching the dates
  all_data <- list()
  available_dates <- c()

  for (i in seq_along(date_strings)) {
    date_str <- date_strings[i]
    date_file <- path(species_dir, paste0("date=", date_str, ".parquet"))

    if (file.exists(date_file)) {
      # Read data
      df <- read_parquet(date_file)
      all_data[[length(all_data) + 1]] <- df
      available_dates <- c(available_dates, dates[i])
    }
  }

  # Combine all data
  if (length(all_data) == 0) {
    log_warn(paste(
      "No data available for", scientific_name, "in the date range",
      as.character(start_date), "to", as.character(end_date)
    ))
    return(list(data = NULL, dates = NULL))
  }

  combined_data <- bind_rows(all_data)

  return(list(
    data = combined_data,
    dates = available_dates
  ))
}

#' Create a frame data structure for a specific date
#'
#' @param data Dataframe with bird observation data
#' @param date Date to filter for
#' @return A list with frame data structure
#' @export
create_frame_data <- function(data, date) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  # Filter for specific date
  date_data <- data |>
    filter(date == as.Date(date))

  if (nrow(date_data) == 0) {
    return(NULL)
  }

  # Create raster from points
  # This is a simplified approach - you may need to adjust based on your needs
  grid_size <- 0.05 # Degrees, adjust as needed

  # Determine grid boundaries
  x_range <- range(date_data$longitude)
  y_range <- range(date_data$latitude)

  # Create empty raster grid
  r <- raster(
    xmn = x_range[1] - grid_size,
    xmx = x_range[2] + grid_size,
    ymn = y_range[1] - grid_size,
    ymx = y_range[2] + grid_size,
    resolution = grid_size
  )

  # Set CRS to WGS84
  crs(r) <- "+proj=longlat +datum=WGS84 +no_defs"

  # Create a spatial points dataframe
  pts <- st_as_sf(
    date_data,
    coords = c("longitude", "latitude"),
    crs = st_crs(4326)
  )

  # Rasterize the points (simplified approach)
  # In a real implementation, you might use terra::rasterize or similar

  # Return frame data structure
  list(
    date = as.Date(date),
    raster = r,
    values = date_data$intensity,
    points = pts,
    raw_data = date_data
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
      ext <- raster::extent(
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
      r <- raster::raster(ext, resolution = 0.05)
      print(paste("Empty raster dimensions:", ncol(r), "x", nrow(r)))

      # Set coordinate reference system to WGS84 (EPSG:4326)
      raster::crs(r) <- "EPSG:4326"

      # Rasterize points
      print("Rasterizing points...")
      r <- raster::rasterize(
        points_df[, c("x", "y")],
        r,
        field = points_df$value,
        fun = max
      )

      # Check result
      vals <- raster::values(r)
      non_na_count <- sum(!is.na(vals))
      print(paste("Final raster contains", non_na_count, "non-NA cells"))

      # Validate that we have data in the raster
      if (non_na_count == 0) {
        print("WARNING: Raster has no valid values!")

        # Try alternative method
        print("Trying alternative rasterization method")
        r_terra <- terra::rast(ext, resolution = 0.05)
        terra::crs(r_terra) <- "EPSG:4326"

        # Convert points_df to sf object
        points_sf <- sf::st_as_sf(points_df, coords = c("x", "y"), crs = 4326)

        # Rasterize with terra
        r_terra <- terra::rasterize(points_sf, r_terra, field = "value")

        # Check result
        vals_terra <- terra::values(r_terra)
        non_na_count_terra <- sum(!is.na(vals_terra))
        print(paste("Alternative method: raster contains", non_na_count_terra, "non-NA cells"))

        if (non_na_count_terra > 0) {
          print("Using terra raster instead")
          return(r_terra)
        }
      }

      return(r)
    },
    error = function(e) {
      print(paste("ERROR in points_to_raster:", e$message))
      return(NULL)
    }
  )
}

#' Convert raster to points dataframe
#'
#' @param r Raster object
#' @param date Date for the data
#' @return Data frame with x, y coordinates, value, and date
#' @export
raster_to_points_df <- function(r, date = NULL) {
  print("Converting raster to points dataframe for storage")

  tryCatch(
    {
      # Get non-NA cells
      vals <- terra::values(r)
      non_na_count <- sum(!is.na(vals))
      print(paste("Raster has", non_na_count, "non-NA cells"))

      if (non_na_count == 0) {
        return(data.frame())
      }

      # Try different methods
      if (inherits(r, "SpatRaster")) {
        # Method 1: Using terra
        print("Using terra method")
        df <- as.data.frame(r, xy = TRUE)
        names(df) <- c("x", "y", "value")
        df <- df[!is.na(df$value), ]
      } else {
        # Method 2: Using raster package
        print("Using raster package method")
        r_raster <- raster::raster(r)
        df <- raster::rasterToPoints(r_raster) |> as.data.frame()
        names(df) <- c("x", "y", "value")
      }

      print(paste("Extracted", nrow(df), "points from raster"))

      # Add date column if provided
      if (!is.null(date)) {
        df$date <- date
      }

      return(df)
    },
    error = function(e) {
      print(paste("Error in raster_to_points_df:", e$message))
      return(data.frame())
    }
  )
}

#' Get data from a Parquet file or create one if it doesn't exist
#'
#' @param scientific_name Scientific name of the bird species
#' @param date Date for the data
#' @param data_dir Directory to store Parquet files
#' @return Data frame with observation data
#' @noRd
get_or_create_parquet <- function(scientific_name, date, data_dir) {
  # Format date for filenames
  date_str <- as.character(as.Date(date))

  # Build path to Parquet file
  parquet_file <- path(data_dir, paste0("date=", date_str, ".parquet"))

  # Debug logging for parquet path
  print(paste("Checking for parquet file at:", parquet_file))

  # Check if directory exists, create if not
  dir_create(data_dir, recurse = TRUE)

  # Check if Parquet file exists
  if (file.exists(parquet_file)) {
    print(paste("Found existing parquet file:", parquet_file))

    # Read the Parquet file
    tryCatch(
      {
        df <- arrow::read_parquet(parquet_file)
        print(paste("Successfully read parquet file with", nrow(df), "rows"))

        # Check if the dataframe has the expected columns
        expected_cols <- c("x", "y", "value", "date")
        missing_cols <- setdiff(expected_cols, names(df))

        if (length(missing_cols) > 0) {
          print(paste("WARNING: Parquet file missing columns:", paste(missing_cols, collapse = ", ")))

          # Add missing date column if needed
          if ("date" %in% missing_cols) {
            print("Adding missing date column")
            df$date <- date
          }
        }

        return(df)
      },
      error = function(e) {
        print(paste("Error reading parquet file:", e$message))
        # If read fails, try downloading instead
        return(download_and_create_parquet(scientific_name, date, data_dir, parquet_file))
      }
    )
  } else {
    print("Parquet file not found, downloading data...")
    return(download_and_create_parquet(scientific_name, date, data_dir, parquet_file))
  }
}

# Helper function to download and create a parquet file
download_and_create_parquet <- function(scientific_name, date, data_dir, parquet_file) {
  # Get the cache directory for TIF files
  cache_dir <- path("app/data/rtbm/cache")

  # Download raster data
  raster_data <- download_and_process_raster(date, scientific_name, cache_dir)

  if (!is.null(raster_data)) {
    print("Downloaded raster data successfully")

    # Create points from raster
    points_df <- raster_to_points_df(raster_data, date)
    print(paste("Converted raster to", nrow(points_df), "points"))

    if (nrow(points_df) > 0) {
      # Write to Parquet
      print(paste("Writing", nrow(points_df), "points to parquet:", parquet_file))
      tryCatch(
        {
          arrow::write_parquet(points_df, parquet_file)
          print("Successfully wrote parquet file")
          return(points_df)
        },
        error = function(e) {
          print(paste("Error writing parquet file:", e$message))
          return(points_df) # Return data even if saving failed
        }
      )
    } else {
      print("No points to save")
      return(NULL)
    }
  } else {
    print("Failed to download raster data")
    return(NULL)
  }
}

#' Update local data cache
#'
#' @param days_back Number of days back to process
#' @return TRUE if update was successful, FALSE otherwise
#' @export
update_local_cache <- function(days_back = 14) {
  # Initialize directories
  dirs <- init_storage_dirs()

  # Load bird species info
  bird_info <- load_bird_species()

  # Process recent data
  success <- process_recent_data(
    days_back = days_back,
    bird_info = bird_info,
    dirs = dirs
  )

  return(success)
}

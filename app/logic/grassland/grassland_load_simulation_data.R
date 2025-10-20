box::use(
  readr[read_delim],
  app /
    logic /
    grassland /
    grassland_soil_management_data_load[
      read_project_config,
      get_soil_file_name,
      get_management_file_name,
      read_soil_shares,
      read_soil_data_table,
      read_management_data_table
    ],
  app / logic / grassland / grassland_read_data_weather[read_weather_data],
  app / logic / grassland / grassland_read_data_grass[read_grass_simulations],
)

#' Load all grassland simulation data from a single run directory
#'
#' This function provides a unified interface to load all types of data
#' associated with a grassland simulation run, including grass output,
#' weather data, management actions, soil information, and metadata.
#'
#' @param run_dir Character. Path to the simulation run directory or grassland base directory.
#' @param lat Numeric. Latitude with up to 6 decimal places.
#' @param lon Numeric. Longitude with up to 6 decimal places.
#' @param start_year Integer. Start year of the simulation period.
#' @param end_year Integer. End year of the simulation period.
#' @param run_number Integer. Sequential run number for this simulation session.
#'
#' @return A list containing:
#'   \item{grass_output}{Data frame with grass simulation results (PFT fractions)}
#'   \item{weather_data}{List of weather series (Precipitation, Temperature, PAR, PET)}
#'   \item{management_data}{Data frame with management actions and dates}
#'   \item{soil_data}{Data frame with soil properties by type}
#'   \item{soil_shares}{Named vector of soil type proportions}
#'   \item{metadata}{List with run_dir, run_number, project_name, lat, lon, start_year, end_year, load_timestamp}
#'
#' @export
load_grassland_simulation_data <- function(
  run_dir,
  lat,
  lon,
  start_year,
  end_year,
  run_number
) {
  # Generate lat/lon name for file paths (format: lat51.391900_lon11.878700)
  lat_lon_name <- sprintf("lat%.6f_lon%.6f", lat, lon)

  # Generate project name from coordinates (format: project_lat51.391900_lon11.878700)
  project_name <- paste0("project_", lat_lon_name)

  # Initialize result list
  result <- list(
    grass_output = NULL,
    weather_data = NULL,
    management_data = NULL,
    soil_data = NULL,
    soil_shares = NULL,
    metadata = list(
      run_dir = run_dir,
      run_number = run_number,
      project_name = project_name,
      lat = lat,
      lon = lon,
      start_year = start_year,
      end_year = end_year,
      load_timestamp = Sys.time()
    )
  )

  # Try to load each data type, catching errors individually
  # so partial success is possible

  # 1. Load project configuration
  tryCatch(
    {
      project_conf <- read_project_config(project_name)
      result$metadata$project_config <- project_conf
    },
    error = function(e) {
      warning("Failed to load project config: ", e$message)
      result$metadata$project_config <- NULL
    }
  )

  # 2. Load management data
  tryCatch(
    {
      # Try scenarios structure first, then project structure
      scenarios_mgmt_dir <- file.path(run_dir, "scenarios", lat_lon_name, "management")
      if (dir.exists(scenarios_mgmt_dir)) {
        # Find management file - prefer GER_Schwieder as per original logic
        mgmt_files <- list.files(
          path = scenarios_mgmt_dir,
          pattern = paste0(lat_lon_name, ".*__management__GER_Schwieder\\.txt$"),
          full.names = TRUE
        )
        if (length(mgmt_files) > 0) {
          result$management_data <- read_management_data_table(mgmt_files[1])
        } else {
          # Fall back to any management file if GER_Schwieder not found
          mgmt_files <- list.files(
            path = scenarios_mgmt_dir,
            pattern = paste0(lat_lon_name, ".*__management__.*\\.txt$"),
            full.names = TRUE
          )
          if (length(mgmt_files) > 0) {
            # Exclude data_query_protocol files
            mgmt_files <- mgmt_files[!grepl("data_query_protocol", mgmt_files)]
            if (length(mgmt_files) > 0) {
              result$management_data <- read_management_data_table(mgmt_files[1])
            }
          }
        }
      } else {
        # Try old project-based structure
        management_file <- get_management_file_name(project_name)
        result$management_data <- read_management_data_table(management_file)
      }
    },
    error = function(e) {
      warning("Failed to load management data: ", e$message)
      result$management_data <- NULL
    }
  )

  # 3. Load soil shares
  tryCatch(
    {
      # Try scenarios structure first, then project structure
      scenarios_soil_dir <- file.path(run_dir, "scenarios", lat_lon_name, "soil")
      if (dir.exists(scenarios_soil_dir)) {
        # Find soil file
        soil_files <- list.files(
          path = scenarios_soil_dir,
          pattern = paste0(lat_lon_name, ".*__soil\\.txt$"),
          full.names = TRUE
        )
        if (length(soil_files) > 0) {
          # Use read_soil_shares which reads the first line properly
          result$soil_shares <- read_soil_shares(soil_files[1])
        }
      } else {
        # Try old project-based structure
        result$soil_shares <- read_soil_shares(project_name)
      }
    },
    error = function(e) {
      warning("Failed to load soil shares: ", e$message)
      result$soil_shares <- NULL
    }
  )

  # 4. Load soil data table
  tryCatch(
    {
      # Try scenarios structure first, then project structure
      scenarios_soil_dir <- file.path(run_dir, "scenarios", lat_lon_name, "soil")
      if (dir.exists(scenarios_soil_dir)) {
        # Find soil file
        soil_files <- list.files(
          path = scenarios_soil_dir,
          pattern = paste0(lat_lon_name, ".*__soil\\.txt$"),
          full.names = TRUE
        )
        if (length(soil_files) > 0) {
          result$soil_data <- read_soil_data_table(soil_files[1])
        }
      } else {
        # Try old project-based structure
        soil_file <- get_soil_file_name(project_name)
        result$soil_data <- read_soil_data_table(soil_file)
      }
    },
    error = function(e) {
      warning("Failed to load soil data: ", e$message)
      result$soil_data <- NULL
    }
  )

  # 5. Load weather data
  tryCatch(
    {
      # Check if run_dir has scenarios/ subdirectory (grassland base structure)
      # or if it's a direct run directory
      scenarios_dir <- file.path(run_dir, "scenarios", lat_lon_name, "weather")
      if (dir.exists(scenarios_dir)) {
        # Grassland base structure: run_dir/scenarios/lat_lon/weather/
        # Find any weather file matching the lat_lon pattern
        weather_files <- list.files(
          path = scenarios_dir,
          pattern = paste0(lat_lon_name, ".*__weather\\.txt$"),
          full.names = TRUE
        )
        if (length(weather_files) > 0) {
          # Use string format for end_date to match old code
          end_date <- paste0(end_year, "-12-31")
          colors <- c("#0072B2", "#ae4d18", "#956618", "#108039", "#56B4E9")
          result$weather_data <- read_weather_data(
            file_path = weather_files[1],
            end_date = end_date,
            colors = colors
          )
          result$metadata$weather_file <- weather_files[1]
        } else {
          warning("No weather files found in: ", scenarios_dir)
          result$weather_data <- NULL
        }
      } else {
        # Simple run directory structure - try exact match first, then pattern
        weather_file <- file.path(
          run_dir,
          paste0(lat_lon_name, "__", start_year, "-01-01_", end_year, "-12-31__weather.txt")
        )

        if (file.exists(weather_file)) {
          end_date <- paste0(end_year, "-12-31")
          colors <- c("#0072B2", "#ae4d18", "#956618", "#108039", "#56B4E9")
          result$weather_data <- read_weather_data(
            file_path = weather_file,
            end_date = end_date,
            colors = colors
          )
          result$metadata$weather_file <- weather_file
        } else {
          # Try finding any weather file matching pattern
          weather_files <- list.files(
            path = run_dir,
            pattern = paste0(lat_lon_name, ".*__weather\\.txt$"),
            full.names = TRUE
          )
          if (length(weather_files) > 0) {
            end_date <- paste0(end_year, "-12-31")
            colors <- c("#0072B2", "#ae4d18", "#956618", "#108039", "#56B4E9")
            result$weather_data <- read_weather_data(
              file_path = weather_files[1],
              end_date = end_date,
              colors = colors
            )
            result$metadata$weather_file <- weather_files[1]
          } else {
            warning("Weather file not found: ", weather_file)
            result$weather_data <- NULL
          }
        }
      }
    },
    error = function(e) {
      warning("Failed to load weather data: ", e$message)
      result$weather_data <- NULL
    }
  ) # 6. Load grass simulation output
  tryCatch(
    {
      # Check if run_dir has simulations/ subdirectory (grassland base structure)
      # Try both project1 and project_lat_lon naming
      simulations_dir_project1 <- file.path(run_dir, "simulations", "project1", "output")
      simulations_dir_latlon <- file.path(run_dir, "simulations", project_name, "output")

      if (dir.exists(simulations_dir_project1)) {
        # Grassland base structure with project1: run_dir/simulations/project1/output/
        search_dir <- simulations_dir_project1
      } else if (dir.exists(simulations_dir_latlon)) {
        # Grassland base structure with lat_lon: run_dir/simulations/project_lat_lon/output/
        search_dir <- simulations_dir_latlon
      } else {
        # Simple run directory structure
        search_dir <- run_dir
      }

      # Find grass output files (pattern: *__output*__*.txt)
      # We want outputPFT files (new format) or output files (old format) which have the PFT and Fraction columns
      # Try new format first (outputPFT), then fall back to old format (output)
      grass_files <- list.files(
        path = search_dir,
        pattern = paste0(lat_lon_name, ".*__outputPFT__.*\\.txt$"),
        full.names = TRUE
      )

      # If no outputPFT files found, try old format
      if (length(grass_files) == 0) {
        grass_files <- list.files(
          path = search_dir,
          pattern = paste0(lat_lon_name, ".*__output__.*\\.txt$"),
          full.names = TRUE
        )
        # Exclude outputPlant and outputCommunity files from old format
        grass_files <- grass_files[!grepl("outputPlant|outputCommunity", grass_files)]
      }

      if (length(grass_files) > 0) {
        # Store all grass file paths for use with original chart functions
        result$metadata$grass_files <- grass_files
        # Store end_date as string to match old code format
        result$metadata$grass_end_date <- paste0(end_year, "-12-31")

        # Use the first grass file - read_grass_simulations returns series list
        grass_file <- grass_files[1]
        # read_grass_simulations expects specific parameters
        grass_series <- read_grass_simulations(
          filename = grass_file,
          plot_type = "line",
          colors = c("#18A547", "#AF2C6E", "#422CAF"),
          stack = NULL,
          file_nr = NULL
        )

        # Convert series list back to data frame format for compatibility
        # Extract data from series and reconstruct data frame
        if (length(grass_series) > 0) {
          # Read the file directly to get the data frame
          # Use cols_only to read just the columns we need
          grass_data <- readr::read_delim(
            file = grass_file,
            skip = 0,
            trim_ws = TRUE,
            delim = "\t",
            escape_double = FALSE,
            col_names = TRUE,
            col_types = readr::cols_only(
              Date = readr::col_date(format = ""),
              DayCount = readr::col_integer(),
              PFT = readr::col_integer(),
              Fraction = readr::col_double(),
              NumberPlants = readr::col_integer()
            ),
            show_col_types = FALSE
          )
          result$grass_output <- grass_data
        }
      } else {
        warning("No grass output files found in: ", search_dir)
        result$grass_output <- NULL
      }
    },
    error = function(e) {
      warning("Failed to load grass output: ", e$message)
      result$grass_output <- NULL
    }
  )

  return(result)
}

library(readr)
library(stringr)

read_project_config <- function(
    project_name = "project1"
) {
  simulations_path <- "app/data/grassland/simulations/"
  project_path <- paste0(simulations_path, project_name, "/")
  
  config_file_path <- list.files(project_path, pattern = "(__configuration__generic_)")
  
  project_config_file <- read_lines(paste0(project_path, config_file_path))
  return(project_config_file)
}

get_weather_file_name <- function(
    project_config_file,
    skip_no_lines = 33,
    max_no_printed = 33
) {
  cutoff_lines <- project_config_file[skip_no_lines:max_no_printed]
  
  weather_file_name <- cutoff_lines[cutoff_lines |>
                                      str_detect("^weatherFile\t")] |>
    str_remove("^weatherFile\t")
  
  return(weather_file_name)
}

get_lat_lon_name <- function(
    weather_file_name
) {
  weather_file_lat_lon <- weather_file_name |>
    str_remove("__[0-9-_]+__weather\\.txt")
}

get_weather_file_path <- function(
    lat_lon_name
) {
  scenarios_path <- "app/data/grassland/scenarios/"
  weather_data_path <- paste0(scenarios_path, lat_lon_name, "/weather/")
  
  weather_file_name <- list.files(weather_data_path)[list.files(weather_data_path) |>
                                                       str_detect("__weather\\.txt$")]
  weather_file_path <- paste0(weather_data_path, weather_file_name)
}

read_weather_data <- function(file_path) {
  dt <- read_delim(
    file = file_path,
    delim = "\t",
    col_names = TRUE,
    skip = 0,
    show_col_types = FALSE,
    id = NULL,
  )
  return(dt)
}

weather_file <- get_weather_file_name(read_project_config())
lat_lon <- get_lat_lon_name(weather_file)
weather_filepath <- get_weather_file_path(lat_lon)

data_weather <- read_weather_data(weather_filepath)


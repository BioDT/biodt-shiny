box::use(
  readr[read_lines, read_delim],
  stringr[str_detect, str_remove],
  config,
)

#' @export
read_project_config <- function(
  project_name = "project1"
) {
  simulations_path <- file.path(config$get("data_path"), "grassland", "simulations")
  project_path <- file.path(simulations_path, project_name)

  config_file_path <- list.files(project_path, pattern = "(__configuration__generic_)")

  project_config_file <- read_lines(file.path(project_path, config_file_path))
  return(project_config_file)
}

# file name of daily weather data, retreived from configuration txt file
# only the name of file without path, the path is constructed w/ get_lat_lon_name(), get__file_path(), etc
#' @export
get_soil_file_name <- function(
  project_config_file,
  skip_no_lines = 36,
  max_no_printed = 36
) {
  cutoff_lines <- project_config_file[skip_no_lines:max_no_printed]

  soil_file_name <- cutoff_lines[
    cutoff_lines |>
      str_detect("^soilFile\t")
  ] |>
    str_remove("^soilFile\t")

  return(soil_file_name)
}

# file name of management actions (sowing, mowing, fertilization, irrigation), retreived from configuration txt file
# the file itself is then located in <data_path>/grassland/scenarios/<latXX.XXXXXX_lonYY.YYYYYY>/management/
# only the name of file without path, the path is constructed w/ get_lat_lon_name(), get__file_path(), etc
#' @export
get_management_file_name <- function(
  project_config_file,
  skip_no_lines = 39,
  max_no_printed = 39
) {
  cutoff_lines <- project_config_file[skip_no_lines:max_no_printed]

  management_file_name <- cutoff_lines[
    cutoff_lines |>
      str_detect("^managementFile\t")
  ] |>
    str_remove("^managementFile\t")

  return(management_file_name)
}

#' @export
get_lat_lon_name <- function(
  type_of_input_file = c("management", "soil"),
  file_name
) {
  type = ifelse(type_of_input_file == "management", "management", "soil")
  file_lat_lon <- file_name |>
    str_remove(paste0("__[0-9]+__", type, "\\.txt"))
  return(file_lat_lon)
}

#' @export
get_file_path <- function(
  type_of_input_file = c("management", "soil"),
  lat_lon_name
) {
  scenarios_path <- file.path(config$get("data_path"), "grassland", "scenarios")
  data_path <- paste0(file.path(scenarios_path, lat_lon_name, type_of_input_file), .Platform$file.sep)

  filename_regexp_ending <- ifelse(
    type_of_input_file == "management",
    "__management__GER_Schwieder\\.txt$",
    "__soil\\.txt$"
  )
  file_name <- list.files(data_path)[
    list.files(data_path) |>
      str_detect(filename_regexp_ending)
  ]
  file_path <- file.path(data_path, file_name)
  return(file_path)
}

#' @export
read_main_three_values <- function(file_path) {
  silt_clay_sand <- read_delim(
    file = file_path,
    delim = "\t",
    col_names = TRUE,
    col_types = list(
      Silt = "c",
      Clay = "c",
      Sand = "c"
    ),
    skip = 0,
    n_max = 1,
    show_col_types = FALSE,
  )
  return(silt_clay_sand)
}

#' @export
read_soil_data_table <- function(file_path) {
  dt <- read_delim(
    file = file_path,
    delim = "\t",
    col_names = TRUE,
    skip = 3,
    show_col_types = FALSE,
    id = NULL,
  )
  return(dt)
}

#'@export
read_management_data_table <- function(file_path) {
  dt <- read_delim(
    file = file_path,
    delim = "\t",
    col_names = TRUE,
    # show_col_types = FALSE,
  )
  return(dt)
}

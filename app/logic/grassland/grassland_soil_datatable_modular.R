box::use(
  readr[read_lines, read_delim],
  stringr[str_detect, str_remove]
)

#' @export
read_project_config <- function(
  project_name = "project1"
) {
  simulations_path <- "app/data/grassland/simulations/"
  project_path <- paste0(simulations_path, project_name, "/")

  config_file_path <- list.files(project_path, pattern = "(__configuration__generic_)")

  project_config_file <- read_lines(paste0(project_path, config_file_path))
  return(project_config_file)
}

#' @export
get_soil_file_name <- function(
  project_config_file,
  skip_no_lines = 36,
  max_no_printer = 36
) {
  cutoff_lines <- project_config_file[skip_no_lines:max_no_printer]

  soil_file_name <- cutoff_lines[cutoff_lines |>
    str_detect("^soilFile\t")] |>
      str_remove("^soilFile\t")

  return(soil_file_name)
}

#' @export
get_lat_lon_name <- function(
  soil_file_name
) {
  soil_file_lat_lon <- soil_file_name |>
    str_remove("__[0-9]+__soil\\.txt")
}

#' @export
get_soil_file_path <- function(
  lat_lon_name
) {
  scenarios_path <- "app/data/grassland/scenarios/"
  soil_data_path <- paste0(scenarios_path, lat_lon_name, "/soil/")

  soil_file_name <- list.files(soil_data_path)[list.files(soil_data_path) |>
    str_detect("__soil\\.txt$")]
  soil_file_path <- paste0(soil_data_path, soil_file_name)
}

#' @export
read_main_three_values <- function(){}

#' @export
read_data_table <- function(file_path) {
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

box::use(
  readr[read_lines],
)

#' @export
read_project_config <- function(
  project_name = "project1",
  skip_no_lines = 0, # TODO move to separat fn
  max_no_lines = 30  # dtto 
) {
  simulations_path <- "app/data/grassland/simulations/"
  project_path <- paste0(simulations_path, project_name, "/")

  config_file_path <- list.files(project_path, pattern = "(__configuration__generic_)")

  project_config <- read_lines(paste0(project_path, config_file_path))
  print(project_config)
}
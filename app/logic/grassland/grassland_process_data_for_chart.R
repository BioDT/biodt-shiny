box::use(
  readr[read_delim],
  echarty,
  dplyr
)

grassland_pft_column_types <- list(
  Date = "D",
  DayCount = "i",
  PFT = "i",
  Fraction = "d",
  NumberPlants = "i"
)

#' @export
grassland_data_plot <- function(filename,
  stack = 'total',
  type = 'bar',
  grassland_pft_column_types
) {
  
  colors <- c('#008000', '#800000', '#000080')

  dir_with_grasslands_data <- "app/data/grassland"
  dir_project1_example <- paste0("app/data/grassland", "/simulations/project1/output")
  filepaths_results <- list.files(dir_project1_example, full.names = TRUE)
  
  input_data <- read_delim(
    file = filename,
    skip = 0,
    trim_ws = TRUE,
    delim = "\t",
    escape_double = FALSE,
    col_names = TRUE,
    col_types = column_types
  )

  simulations <- NULL
  for (i in 1:2){#length(filepaths_results)) {
    filepath <- filepaths_results[i]
    simulations <- simulations |>
      c(process_grassland_data(filepath, i))
  }

  time <- input_data <- readr::read_delim(
    file = filepaths_results[1],
    skip = 0,
    trim_ws = TRUE,
    delim = "\t",
    escape_double = FALSE,
    col_names = TRUE,
    col_types = list(
      Date = "D",
      DayCount = "-",
      PFT = "-",
      Fraction = "-",
      NumberPlants = "-"
    )
  )$Date |> unique()

  kl <- length(simulations) - 1 
  formatter <- paste0('{a',kl,'} at time {b', kl,'} <br /> {c',kl,'}')

  series_list <- list()  
  for (pft in unique(input_data$PFT)) {
    series_list[[length(series_list) + 1]] <- 
        list(name = paste0("PFT", pft),
             type = type,
             stack = stack,
             color = colors[pft],
             symbol = "none",
             showSymbol = FALSE,
             emphasis = list(disabled = TRUE),
             data = unname(as.list(unlist(input_data[input_data$PFT == pft, "Fraction"]))))
  }
  
  return(series_list)
}




# time <- input_data <- readr::read_delim(
#   file = filepaths_results[1],
#   skip = 0,
#   trim_ws = TRUE,
#   delim = "\t",
#   escape_double = FALSE,
#   col_names = TRUE,
#   col_types = list(
#     Date = "D",
#     DayCount = "-",
#     PFT = "-",
#     Fraction = "-",
#     NumberPlants = "-"
#   )
# )$Date |> unique()

# simulations <- purrr::map(filepaths_results,
#                           read_grass_simulations)

# Prepare tooltip formatter
# kl <- length(simulations) - 1 
# formatter <- paste0('{a',kl,'} at time {b', kl,'} <br /> {c',kl,'}')
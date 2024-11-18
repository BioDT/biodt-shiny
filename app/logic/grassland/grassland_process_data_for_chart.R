box::use(
  readr[read_delim]
)

dir_with_grasslands_data <- "app/data/grassland"
dir_project1_example <- paste0("app/data/grassland", "/simulations/project1/output")

get_paths_of_all_files <- list.files(dir_project1_example, full.names = TRUE)

#' export
read_grass_simulations <- function(filename,
                                   stack,
                                   column_types = list(
                                     Date = "D",
                                     DayCount = "i",
                                     PFT = "i",
                                     Fraction = "d",
                                     NumberPlants = "i"
                                   )) {
  
  colors <- c('#008000', '#800000', '#000080')
  
  input_data <- read_delim(
    file = filename,
    skip = 0,
    trim_ws = TRUE,
    delim = "\t",
    escape_double = FALSE,
    col_names = TRUE,
    col_types = column_types
  )
  
  series_list <- list()
  
  for (pft in unique(input_data$PFT)) {
    series_list[[length(series_list) + 1]] <- 
        list(name = paste0("PFT", pft),
             type = "bar",
             stack = stack,
             color = colors[pft],
             symbol = "none",
             showSymbol = FALSE,
             emphasis = list(disabled = TRUE),
             data = unname(as.list(unlist(input_data[input_data$PFT == pft, "Fraction"]))))
  }
  
  
  return(series_list)
}
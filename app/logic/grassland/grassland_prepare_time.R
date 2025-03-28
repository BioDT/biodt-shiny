box::use(
  readr[read_delim],
  dplyr[pull]
)

#' @export
get_time <- function(
  file
) {
  time <- read_delim(
    file = file,
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
  ) |>
    pull(Date) |>
    unique()
}

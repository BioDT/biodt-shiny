box::use(
  readr[read_delim],
  dplyr[filter]
)

# loads and restructure WEATHER data ----
#' @export
read_weather_data <- function(
  file_path,
  end_date,
  colors
) {
  input_data <- read_delim(
    file = file_path,
    delim = "\t",
    col_names = TRUE,
    skip = 0,
    show_col_types = FALSE,
    id = NULL,
  ) |>
    dplyr::filter(Date <= end_date)

  series <- list()

  series[[1]] <-
    list(
      name = "Precipitation[mm]",
      type = "line",
      color = colors[1],
      symbol = "none",
      showSymbol = FALSE,
      emphasis = list(disabled = TRUE),
      xAxisIndex = 1,
      yAxisIndex = 1,
      data = unname(as.list(unlist(input_data[, "Precipitation[mm]"])))
    )
  series[[2]] <-
    list(
      name = "Temperature[degC]",
      type = "line",
      color = colors[2],
      symbol = "none",
      showSymbol = FALSE,
      emphasis = list(disabled = TRUE),
      xAxisIndex = 2,
      yAxisIndex = 2,
      data = unname(as.list(unlist(input_data[, "Temperature[degC]"])))
    )
  series[[3]] <-
    list(
      name = "Temperature_Daylight[degC]",
      type = "line",
      color = colors[3],
      symbol = "none",
      showSymbol = FALSE,
      emphasis = list(disabled = TRUE),
      xAxisIndex = 2,
      yAxisIndex = 2,
      data = unname(as.list(unlist(input_data[, "Temperature_Daylight[degC]"])))
    )
  series[[4]] <-
    list(
      name = "PAR[µmolm-2s-1]",
      type = "line",
      color = colors[4],
      symbol = "none",
      showSymbol = FALSE,
      emphasis = list(disabled = TRUE),
      xAxisIndex = 3,
      yAxisIndex = 3,
      data = unname(as.list(unlist(input_data[, "PAR[µmolm-2s-1]"])))
    )
  series[[5]] <-
    list(
      name = "Daylength[h]",
      type = "line",
      color = colors[5],
      symbol = "none",
      showSymbol = FALSE,
      emphasis = list(disabled = TRUE),
      xAxisIndex = 4,
      yAxisIndex = 4,
      data = unname(as.list(unlist(input_data[, "Daylength[h]"])))
    )
  series[[6]] <-
    list(
      name = "PET[mm]",
      type = "line",
      color = colors[6],
      symbol = "none",
      showSymbol = FALSE,
      emphasis = list(disabled = TRUE),
      xAxisIndex = 5,
      yAxisIndex = 5,
      data = unname(as.list(unlist(input_data[, "PET[mm]"])))
    )

  return(series)
}

# weather_col_names <- c(
#   "Precipitation[mm]",
#   "Temperature[degC]",
#   "Temperature_Daylight[degC]",
#   "PAR[µmolm-2s-1]",
#   "Daylength[h]",
#   "PET[mm]"
# )

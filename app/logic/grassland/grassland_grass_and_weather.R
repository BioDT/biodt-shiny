library(readr)
library(dplyr)
library(echarty)
library(readr)
library(stringr)
library(viridisLite)
library(purrr)

read_grass_simulations <- function(
    filename,
    column_types = list(
      Date = "D",
      DayCount = "i",
      PFT = "i",
      Fraction = "d",
      NumberPlants = "i"
    ),
    plot_type = c("bar", "line"),
    colors = c("#00aa00", "#a00000", "#0000a0"),
    stack = NULL,
    series_opacity = 0.2) {

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
  pft_list <- sort(unique(input_data$PFT))
  for (i in seq_along(pft_list)) {
    series_list[[length(series_list) + 1]] <-
      list(
        name = paste("PFT", pft_list[i]),
        type = plot_type[1],
        stack = stack,
        color = colors[i],
        symbol = "none",
        showSymbol = FALSE,
        emphasis = list(disabled = TRUE),
        data = unname(as.list(unlist(input_data[input_data$PFT == pft_list[i], "Fraction"])))
      )
  }

  return(series_list)
}

prepare_time <- function(filepaths) {
  time <- read_delim(
    file = filepaths[1],
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
  
  return(time)
}

read_weather_data <- function(
    file_path = "app/data/grassland/scenarios/lat51.391900_lon11.878700/weather/lat51.391900_lon11.878700__2013-01-01_2023-12-31__weather.txt",
    end_date = "2015-12-31",
    colors = colors_for_weather
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
  
  series_list <- list()
  weather_col_names <- c("Precipitation[mm]", "Temperature[degC]", "Temperature_Daylight[degC]", "PAR[µmolm-2s-1]", "Daylength[h]", "PET[mm]")
  for (col_name in weather_col_names) {
    i <- length(series_list) + 1
    
    series_list[[i]] <-
      list(
        name = col_name,
        type = "line",
        color = colors[i],
        symbol = "none",
        showSymbol = FALSE,
        emphasis = list(disabled = TRUE),
        data = unname(as.list(unlist(input_data[, col_name])))
      )
  }
  
  return(series_list)
}

# Grass example execution ----
filepaths_results <- list.files("app/data/grassland/simulations/project1/output/", full.names = TRUE)

grass_data <- read_grass_simulations(filepaths_results[1])
str(grass_data)

# Weather example execution ----
time <- prepare_time(filepaths_results)
weather_filepath <- "app/data/grassland/scenarios/lat51.391900_lon11.878700/weather/lat51.391900_lon11.878700__2013-01-01_2023-12-31__weather.txt"
colors_for_weather <- viridis(6)
grass_end_date <- time[length(time)]

weather_data <- read_weather_data(
  weather_filepath,
  grass_end_date,
  colors = colors_for_weather
)
str(weather_data)


generate_chart <- function(
    filepaths,
    plot_type = "line", # "bar", "line"
    plot_series = "all", # "all", "mean", "series"
    colors = c("#00aa00", "#a00000", "#0000d3"),
    colors_series = c("#b4e4b4", "#dfa7a7", "#9c9cdf"),
    return_series = FALSE) {
  
  if (plot_type == "bar") {
    plot_series <- "mean"
    stack <- "total"
  } else if (plot_type == "line") {
    stack <- NULL
  }
  
  final_simulations <- NULL
  simulations <- NULL
  for (i in 1:length(filepaths)) {
    filepath <- filepaths[i]
    simulations <- simulations |>
      c(
        read_grass_simulations(
          filepath,
          plot_type = plot_type,
          colors = colors_series,
          stack = stack
        )
      )
  }
  
  if (plot_series == "series" || plot_series == "all") {
    final_simulations <- simulations
  }
  
  if (plot_series == "mean" || plot_series == "all") {
    # Compute mean ----
    pft_list <- map_chr(simulations, "name")
    pft_unique <- sort(unique(pft_list))
    
    for (i in seq_along(sort(pft_unique))) {
      sub_simulations <- simulations[pft_list == pft_list[i]]
      n_series <- length(sub_simulations)
      
      series_mean <- rep(0, length(sub_simulations[[1]]$data))
      
      for (series in sub_simulations) {
        series_mean <- series_mean + unlist(series$data) / n_series
      }
      
      series_mean <- series_mean |>
        round(2) |>
        as.list()
      
      final_simulations <- final_simulations |>
        append(list(
          list(
            name = paste(pft_unique[i], "mean"),
            type = plot_type,
            # symbol = "pin",
            showSymbol = FALSE,
            stack = stack,
            symbolSize = 20,
            color = colors[i],
            emphasis = list(disabled = TRUE),
            data = series_mean
          )
        ))
    }
  }
  
  # Prepare time
  time <- read_delim(
    file = filepaths[1],
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
  
  # Prepare tooltip formatter
  kl <- (length(final_simulations) - length(pft_unique)):(length(final_simulations) - 1)
  formatter <- paste0("{a", kl, "} at time {b", kl, "}:  {c", kl, "}", collapse = "<br />")
  
  # Echarty: making chart ----
  #' @export
  chart <- ec.init()
  chart$x$opts <-
    list(
      title = list(text = "Grassland simulation"),
      tooltip = list(
        trigger = "axis",
        formatter = formatter
      ),
      legend = list(data = pft_unique),
      xAxis = list(
        type = "category",
        boundaryGap = TRUE,
        name = "Date",
        nameLocation = "middle",
        nameGap = 25,
        nameTextStyle = list(fontWeight = "bolder"),
        data = time
      ),
      yAxis = list(
        type = "value",
        boundaryGap = FALSE,
        name = "Fraction",
        nameLocation = "middle",
        nameGap = 40,
        nameTextStyle = list(fontWeight = "bolder"),
        min = 0,
        max = 100
      ),
      series = final_simulations
    )
  
  if (return_series) {
    return(final_simulations)
  } else {
    return(chart)
  }
}


# Parameters ----
filepaths_results <- list.files("app/data/grassland/simulations/project1/output/", full.names = TRUE)
plot_type <- "line"
plot_series <- "all"
stack <- "total"
series_opacity <- 0.01
colors <- viridisLite::cividis(3) #c("#00ab4a", "#ae0000", "#003fc8")
colors_series <- viridisLite::magma(3)


# Run -----
generate_chart(
  filepaths = filepaths_results,
  plot_type = "bar",
  plot_series = "series",
  colors_series = colors_series,
  return_series = FALSE
)

################################################################################----
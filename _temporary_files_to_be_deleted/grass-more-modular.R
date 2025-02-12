library(readr)
library(dplyr)
library(purrr)
library(echarty)
library(readr)
library(stringr)

# read_grass_simulations <- function(
#     filename,
#     column_types = list(
#       Date = "D",
#       DayCount = "i",
#       PFT = "i",
#       Fraction = "d",
#       NumberPlants = "i"
#     ),
#     plot_type = c("bar", "line"),
#     colors = c("#00aa00", "#a00000", "#0000a0"),
#     stack = NULL,
#     series_opacity = 0.2) {
#   
#   input_data <- read_delim(
#     file = filename,
#     skip = 0,
#     trim_ws = TRUE,
#     delim = "\t",
#     escape_double = FALSE,
#     col_names = TRUE,
#     col_types = column_types
#   )
#   
#   series_list <- list()
#   pft_list <- sort(unique(input_data$PFT))
#   for (i in seq_along(pft_list)) {
#     series_list[[length(series_list) + 1]] <-
#       list(
#         name = paste("PFT", pft_list[i]),
#         type = plot_type[1],
#         stack = stack,
#         color = colors[i],
#         symbol = "none",
#         showSymbol = FALSE,
#         emphasis = list(disabled = TRUE),
#         data = unname(as.list(unlist(input_data[input_data$PFT == pft_list[i], "Fraction"])))
#       )
#   }
#   
#   return(series_list)
# }

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
colors <- c("#00ab4a", "#ae0000", "#003fc8")


# Run -----
generate_chart(
  filepaths = filepaths_results,
  plot_type = "line",
  plot_series = "mean",
  return_series = FALSE
)

################################################################################


# TWO CHARTs CONNECTED - FIRST ABOVE SECOND - BASIC HOW TOs ----
# it is one of few possible ways how to do it
# SERIES option might look like this ----
# option = {
# series: [
#   // These series will show in the first coordinate, each series map a row in dataset.
#   { type: 'bar', seriesLayoutBy: 'row', xAxisIndex: 0, yAxisIndex: 0 },
#   { type: 'bar', seriesLayoutBy: 'row', xAxisIndex: 0, yAxisIndex: 0 },
#   { type: 'bar', seriesLayoutBy: 'row', xAxisIndex: 0, yAxisIndex: 0 },
#   // These series will show in the second coordinate, each series map a column in dataset.
# WE DON'T WANT TO MAP A COLUMN, BUT SERIES AGAIN ----
#   { type: 'bar', seriesLayoutBy: 'column', xAxisIndex: 1, yAxisIndex: 1 },
#   { type: 'bar', seriesLayoutBy: 'column', xAxisIndex: 1, yAxisIndex: 1 },
#   { type: 'bar', seriesLayoutBy: 'column', xAxisIndex: 1, yAxisIndex: 1 },
#   { type: 'bar', seriesLayoutBy: 'column', xAxisIndex: 1, yAxisIndex: 1 }
# ],
# FOR xAxis, yAxis as well grid option TWO GRIDS MUST BE SET UP ----
# xAxis: [
#   { type: 'category', gridIndex: 0 },
#   { type: 'category', gridIndex: 1 }
# ],
# yAxis: [{ gridIndex: 0 }, { gridIndex: 1 }],
# grid: [{ bottom: '55%' }, { top: '55%' }]
#}

# MORE GRANULAR GRASS FUNCTIONS ----
filepaths_results <- list.files("app/data/grassland/simulations/project1/output/", full.names = TRUE)

read_input_data_pft_each <- function(filename) {
  dat <- read_delim(
    file = filename,
    skip = 0,
    trim_ws = TRUE,
    delim = "\t",
    escape_double = FALSE,
    col_names = TRUE,
    col_types = list(
      Date = "D",
      DayCount = "i",
      PFT = "i",
      Fraction = "d",
      NumberPlants = "i"
    )
  )
  return(dat)
}

make_three_pft_series_lists <- function(input_data) {
  plot_type = c("bar", "line")
  colors = c("#00aa00", "#a00000", "#0000a0")
  stack = NULL
  
  series_list <- list()
  pft_list <- sort(unique(input_data$PFT))
  for (i in seq_along(pft_list)) {
    series_list[[length(series_list) + 1]] <-
      list(
        name = paste("PFT", pft_list[i]),
        type = plot_type[2],
        stack = stack,
        color = colors[i],
        symbol = "none",
        showSymbol = FALSE,
        emphasis = list(disabled = TRUE),
        data = unname(as.list(unlist(input_data[input_data$PFT == pft_list[i], "Fraction"]))),
        seriesLayoutB = 'row',
        xAxisIndex = 0,
        yAxisIndex = 0
      )
  }
  return(series_list)
}

# GRASS DATA LOADED----
data1_grass_raw <- read_input_data_pft_each(filepaths_results[1])
data1_grass_3pft_lists <- make_three_pft_series_lists(data1_raw)

# WEATHER DATA LOADED----
source("load_weather.R")
weather_file <- get_weather_file_name(read_project_config())
lat_lon <- get_lat_lon_name(weather_file)
weather_filepath <- get_weather_file_path(lat_lon)

data_weather <- read_weather_data(weather_filepath)

# SOURCES ----
# https://echarts.apache.org/handbook/en/concepts/dataset/#map-row-or-column-of-dataset-to-series
# https://echarts.apache.org/examples/en/editor.html?c=line-gradient
# https://echarts.apache.org/examples/en/editor.html?c=candlestick-brush it's enough to change value from bars to line on line 227
# https://helgasoft.github.io/echarty/articles/echarty.html echarty docs
# https://r4ds.hadley.nz/joins.html#how-do-joins-work how to intersect data$Date weather && grass

# not used:
# https://rpubs.com/echarty/ecStat here the histogram only


# another ways:
# https://echarts.apache.org/en/api.html#echarts.connect
# https://mirror.math.princeton.edu/pub/CRAN/web/packages/echarty/vignettes/echarty.Rmd search for connect


# TIME - DATE SERIES PREPARATION----
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

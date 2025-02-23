box::use(
  readr[read_delim],
  dplyr[pull],
  purrr[map_chr],
  echarty[ec.init]
)

# loads and restructure GRASSLAND data ----
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

# create 1D vector of DATEs ----
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

# loads and restructure WEATHER data ----
colors_for_weather <- c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF")

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
        xAxisIndex = 1,
        yAxisIndex = 1,
        data = unname(as.list(unlist(input_data[, col_name])))
      )
  }

  return(series_list)
}

# create CHART itself ----
#' @export
generate_chart_with_weather <- function(
  filepaths_grass,
  filepath_weather,
  plot_type = "line", # "bar", "line"
  plot_series = "all", # "all", "mean", "series"
  clrs = c("#00aa00", "#a00000", "#0000d3"),
  colors_for_grass = c("#b4e4b4", "#dfa7a7", "#9c9cdf"),
  colors_for_weather = c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF"),
  grass_end_date = "2015-12-31",
  return_series = FALSE
) {

  if (plot_type == "bar") {
    plot_series <- "mean"
    stack <- "total"
  } else if (plot_type == "line") {
    stack <- NULL
  }

  final_simulations <- NULL
  simulations <- NULL
  for (i in 1:length(filepaths_grass)) {
    filepath <- filepaths_grass[i]
    simulations <- simulations |>
      c(
        read_grass_simulations(
          filepath,
          plot_type = plot_type,
          colors = colors_for_grass,
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
            showSymbol = FALSE,
            stack = stack,
            symbolSize = 20,
            color = clrs[i],
            emphasis = list(disabled = TRUE),
            data = series_mean
          )
        ))
    }
  }


  # generate chart - Weather data ----
  weather_data <- read_weather_data(
    file_path = filepath_weather,
    end_date = grass_end_date,
    colors = colors_for_weather
  )

  final_simulations <- append(simulations, weather_data)

  # Prepare time
  time <- read_delim(
    file = filepaths_grass[1],
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
  #kl <- (length(final_simulations) - length(pft_unique)):(length(final_simulations) - 1)
  kl <- 33:38 # hardcoded, TODO figure out better, with a function or so...

  formatter <- paste0("{a", kl, "}:   {c", kl, "}", collapse = "<br />")
  formatter <- paste0("DATE: {b1}<br />\n", formatter)

  # Echarty: making chart ----
  #' @export
  chart <- ec.init()
  chart$x$opts <-
    list(
      title = list(text = "Grassland Simulation & Weather"),
      tooltip = list(
        trigger = "axis",
        formatter = formatter,
        axisPointer = list(
          type = "cross"
        ),
        borderWidth = 1,
        borderColor = "#ccc",
        padding = 10,
        textStyle = list(color = "#000")
      ),
      axisPointer = list(
        link = list(
          list(
            xAxisIndex = "all"
          )
        ),
        label = list(
          backgroundColor = "#777"
        )
      ),
      legend = list(
        orient = "vertical",
        bottom = "40%",
        right = "2%",
        data = pft_unique
      ),
      grid = list(
        list(
          left = "10%",
          right = "8%",
          height = "50%"
        ),
        list(
          left = "10%",
          right = "8%",
          top = "63%",
          height = "50%"
        )
      ),
      xAxis = list(
        list(
          type = "category",
          scale = TRUE,
          boundaryGap = FALSE,
          axisLine = list(
            onZero = FALSE
          ),
          nameLocation = "middle",
          nameGap = 25,
          nameTextStyle = list(fontWeight = "bolder"),
          data = time
        ),
        list(
          type = "category",
          gridIndex = 1,
          scale = TRUE,
          boundaryGap = FALSE,
          axisLine = list(
            onZero = FALSE
          ),
          axisTick = list(
            show = TRUE
          ),
          splitLine = list(
            show = TRUE
          ),
          axisLabel = list(
            show = TRUE
          ),
          data = time
        )
      ),
      yAxis = list(
        list(
          type = "value",
          boundaryGap = FALSE,
          name = "Fraction",
          nameLocation = "middle",
          nameGap = 40,
          nameTextStyle = list(fontWeight = "bolder"),
          min = 0,
          max = 100,
          scale = TRUE,
          splitArea = list(
            show = TRUE
          )
        ),
        list(
          scale = TRUE,
          gridIndex = 1,
          splitNumber = 2,
          axisLabel = list(
            show = FALSE
          ) ,
          axisLine = list(
            show = FALSE 
          ),
          axisTick = list(
            show = FALSE
          ),
          splitLine = list(
            show = FALSE
          )
        )
      ),
      series = final_simulations
    )

  if (return_series) {
    return(final_simulations)
  } else {
    return(chart)
  }
}


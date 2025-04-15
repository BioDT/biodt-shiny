box::use(
  echarty[ec.init],
  htmlwidgets[JS],
  purrr[map_chr],
)

box::use(
  app / logic / grassland / grassland_read_data_grass[read_grass_simulations],
  app / logic / grassland / grassland_prepare_time[get_time],
  app / logic / grassland / grassland_read_data_weather[read_weather_data]
)

# create CHART with BARs as MEAN of PFTs ----
#' @export
generate_chart_lines_mean <- function(
  filepaths_grass,
  filepath_weather,
  colors_for_grass,
  colors_for_weather,
  grass_end_date
) {
  simulations <- NULL
  for (i in 1:length(filepaths_grass)) {
    filepath <- filepaths_grass[i]
    simulations <- simulations |>
      c(
        read_grass_simulations(
          filename = filepath,
          plot_type = "line",
          colors = colors_for_grass,
          stack = NULL,
          file_nr = NULL
        )
      )
  }

  # Compute mean ----
  # list of all (repeating) names from `simulations` list (33)
  pft_list <- map_chr(simulations, "name")
  # unique names from pft_list (eg. 3 variables)
  pft_unique <- sort(unique(pft_list))
  # init `final_means`
  final_means <- NULL
  # loop through each of three PFTs
  for (i in seq_along(sort(pft_unique))) {
    # subset simulations of the given single PFT (11)
    sub_simulations <- simulations[pft_list == pft_list[i]]
    n_series <- length(sub_simulations)

    # initialize vector `series_mean` with all observations being zero ((1095)
    series_mean <- rep(0, length(sub_simulations[[1]]$data))

    # compute each mean from subset of 11 values
    for (series in sub_simulations) {
      series_mean <- series_mean + unlist(series$data) / n_series
    }

    # round values and change type of `series_mean` from vector to list
    series_mean <- series_mean |>
      round(2) |>
      as.list()

    final_means <- final_means |>
      append(list(
        list(
          name = paste(pft_unique[i], "mean"),
          type = "line",
          showSymbol = FALSE,
          stack = NULL,
          symbolSize = 20,
          color = colors_for_grass[i],
          emphasis = list(disabled = TRUE),
          data = series_mean
        )
      ))
  }

  # Weather data ----
  weather_data <- read_weather_data(
    file_path = filepath_weather,
    end_date = grass_end_date,
    colors = colors_for_weather
  )

  # Prepare time
  time <- get_time(filepaths_grass[1])

  # final list with Grass means and weather, which goes into Echarty
  final <- final_means
  final <- final |>
    append(weather_data)

  chart <- ec.init()
  chart$x$opts <-
    list(
      tooltip = list(
        trigger = "axis",
        formatter = JS(
          "
          function (param) {
            return '<strong>DATE: ' + param[0].name + '</strong><hr size=1 style=\"margin: 6px 0\">' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #18A547\"></i>Mean of PFT 0 - grasses - ' + param.find(item => item.seriesName ==  'PFT 0 mean').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #AF2C6E\"></i>Mean of PFT 1 - forbs - ' + param.find(item => item.seriesName ==  'PFT 1 mean').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #422CAF\"></i>Mean of PFT 2 - legumes - ' + param.find(item => item.seriesName ==  'PFT 2 mean').value +
              '<hr size=1 style=\"margin: 4px 0\">' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #0072B2\"></i>Precipitation [mm]: ' + param.find(item => item.seriesName ==  'Precipitation[mm]').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #D55E00\"></i>Temperature [degC]: ' + param.find(item => item.seriesName ==  'Temperature[degC]').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #E69F00\"></i>Temperature Daylight [degC]: ' + param.find(item => item.seriesName ==  'Temperature_Daylight[degC]').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #42f09f\"></i>PAR [µmolm-2s-1]: ' + param.find(item => item.seriesName ==  'PAR[µmolm-2s-1]').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #56B4E9\"></i>PET [mm]: ' + param.find(item => item.seriesName ==  'PET[mm]').value
          }
        "
        ),
        axisPointer = list(
          type = "cross"
        ),
        borderWidth = 1,
        borderColor = "#ccc",
        padding = 10,
        textStyle = list(color = "#000"),
        backgroundColor = 'rgba(255, 255, 255, 0.8)',
        position = JS(
          "
          function (pos, params, el, elRect, size) {
            var obj = {}

            obj[['left', 'right'][+(pos[0] < size.viewSize[0] / 2)]] = 30;
            
            if (pos[1] < (size.viewSize[1]/2)) {
              obj['top'] = 5
            } else {
              obj[['top', 'bottom'][+(pos[1] > (size.viewSize[1]/2))]] = +(size.viewSize[1] / 3)
            }

            return obj;
          }
        "
        )
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
      grid = list(
        list(left = "10%", right = "8%", top = "5%", height = "16%"),
        list(left = "10%", right = "8%", top = "25%", height = "16%"),
        list(left = "10%", right = "8%", bottom = "41%", height = "16%"),
        list(left = "10%", right = "8%", bottom = "23%", height = "16%"),
        list(left = "10%", right = "8%", bottom = "5%", height = "16%")
      ),
      xAxis = list(
        list(
          type = "category",
          gridIndex = 0,
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
            onZero = TRUE
          ),
          axisTick = list(
            show = TRUE
          ),
          splitLine = list(
            show = TRUE
          ),
          axisLabel = list(
            show = FALSE
          ),
          data = time
        ),
        list(
          type = "category",
          gridIndex = 2,
          scale = TRUE,
          boundaryGap = FALSE,
          axisLine = list(
            onZero = TRUE
          ),
          axisTick = list(
            show = TRUE
          ),
          splitLine = list(
            show = TRUE
          ),
          axisLabel = list(
            show = FALSE
          ),
          data = time
        ),
        list(
          type = "category",
          gridIndex = 3,
          scale = TRUE,
          boundaryGap = FALSE,
          axisLine = list(
            onZero = TRUE
          ),
          axisTick = list(
            show = TRUE
          ),
          splitLine = list(
            show = TRUE
          ),
          axisLabel = list(
            show = FALSE
          ),
          data = time
        ),
        list(
          type = "category",
          gridIndex = 4,
          scale = TRUE,
          boundaryGap = FALSE,
          axisLine = list(
            onZero = TRUE
          ),
          axisTick = list(
            show = TRUE
          ),
          splitLine = list(
            show = TRUE
          ),
          axisLabel = list(
            show = FALSE
          ),
          data = time
        )
      ),
      yAxis = list(
        list(
          name = "Fraction",
          type = "value",
          boundaryGap = FALSE,
          nameLocation = "middle",
          nameGap = 40,
          nameTextStyle = list(fontWeight = "bolder"),
          scale = TRUE,
          gridIndex = 0,
          min = 0,
          max = 100,
          splitArea = list(
            show = TRUE
          )
        ),
        list(
          name = "Precipitation [mm]",
          nameLocation = "middle",
          nameGap = 40,
          nameTextStyle = list(fontWeight = "bolder"),
          scale = TRUE,
          gridIndex = 1,
          splitNumber = 5,
          min = 0,
          max = 50,
          axisLabel = list(
            show = TRUE
          ),
          axisLine = list(
            show = TRUE
          ),
          axisTick = list(
            show = TRUE
          ),
          splitLine = list(
            show = FALSE
          )
        ),
        list(
          name = "Temperature [degC] & \nTemp. Daylight [degC]",
          nameLocation = "middle",
          nameGap = 40,
          nameTextStyle = list(fontWeight = "bolder"),
          scale = TRUE,
          gridIndex = 2,
          splitNumber = 5,
          min = -10,
          max = 40,
          axisLabel = list(
            show = TRUE
          ),
          axisLine = list(
            show = TRUE
          ),
          axisTick = list(
            show = TRUE
          ),
          splitLine = list(
            show = FALSE
          )
        ),
        list(
          name = "PAR [µmolm-2s-1]",
          nameLocation = "middle",
          nameGap = 40,
          nameTextStyle = list(fontWeight = "bolder"),
          scale = TRUE,
          gridIndex = 3,
          splitNumber = 5,
          min = 0,
          max = 1000,
          axisLabel = list(
            show = TRUE
          ),
          axisLine = list(
            show = TRUE
          ),
          axisTick = list(
            show = TRUE
          ),
          splitLine = list(
            show = FALSE
          )
        ),
        list(
          name = "PET [mm]",
          nameLocation = "middle",
          nameGap = 40,
          nameTextStyle = list(fontWeight = "bolder"),
          scale = TRUE,
          gridIndex = 4,
          splitNumber = 5,
          min = 0,
          max = 10,
          axisLabel = list(
            show = TRUE
          ),
          axisLine = list(
            show = TRUE
          ),
          axisTick = list(
            show = TRUE
          ),
          splitLine = list(
            show = FALSE
          )
        )
      ),
      series = final
    )

  return(chart)
}

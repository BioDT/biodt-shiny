box::use(
  echarty[ec.init],
  htmlwidgets[JS],
  utils[str]
)

box::use(
  app / logic / grassland / grassland_read_data_grass[read_grass_simulations],
  app / logic / grassland / grassland_prepare_time[get_time],
  app / logic / grassland / grassland_read_data_weather[read_weather_data]
)

# create CHART with lines for ALL PFTs ----
#' @export
generate_chart_lines <- function(
  filepaths_grass,
  filepath_weather,
  colors_for_grass,
  colors_for_weather,
  grass_end_date = "2015-12-31"
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
          stack = NULL
        )
      )
  }

  # Prepare time
  time <- get_time(filepaths_grass[1])

  # Prepare tooltip formatter
  # kl <- (length(simulations) - length(pft_unique)):(length(simulations) - 1)
  kl <- 0:length(simulations) # hardcoded, TODO figure out better, with a function or so...

  formatter <- paste0("{a", kl, "}:   {c", kl, "}", collapse = "<br />")
  formatter <- paste0("DATE: {b1}<br />\n", formatter)

  # generate chart - Weather data ----
  weather_data <- read_weather_data(
    file_path = filepath_weather,
    end_date = grass_end_date,
    colors = colors_for_weather
  )

  simulations <- simulations |>
    append(weather_data)

  # Echarty: making chart ----
  #' @export
  chart <- ec.init()
  chart$x$opts <-
    list(
      # title = list(text = "Grassland Simulation & Weather"),
      tooltip = list(
        trigger = "axis",
        formatter = formatter,
        # formatter = JS(
        #   "
        #   function (param) {
        #       return '<strong>DATE: ' + param[0].name + '</strong><hr size=1 style=\"margin: 6px 0\">' +
        #         '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #117733\"></i>Mean PFT 0: ' + param.find(item => item.seriesName ==  'PFT 0 mean').value + '<br />' +
        #         '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #882255\"></i>Mean PFT 1: ' + param.find(item => item.seriesName ==  'PFT 1 mean').value + '<br />' +
        #         '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #332288\"></i>Mean PFT 2: ' + param.find(item => item.seriesName ==  'PFT 2 mean').value + '<hr size=1 style=\"margin: 4px 0\">' +
        #         '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #F0E442\"></i>PAR[µmolm-2s-1]: ' + param.find(item => item.seriesName ==  'PAR[µmolm-2s-1]').value + '<hr size=1 style=\"margin: 4px 0\">' +
        #         '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #E69F00\"></i>Precipitation[mm]: ' + param.find(item => item.seriesName ==  'Precipitation[mm]').value + '<br />' +
        #         '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #56B4E9\"></i>Temperature[degC]: ' + param.find(item => item.seriesName ==  'Temperature[degC]').value + '<br />' +
        #         '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #009E73\"></i>Temperature_Daylight[degC]: ' + param.find(item => item.seriesName ==  'Temperature_Daylight[degC]').value + '<br />' +
        #         '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #0072B2\"></i>Daylength[h]: ' + param.find(item => item.seriesName ==  'Daylength[h]').value + '<br />' +
        #         '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #D55E00\"></i>PET[mm]: ' + param.find(item => item.seriesName ==  'PET[mm]').value + '<br />'
        #     }
        # "
        # ),
        axisPointer = list(
          type = "cross"
        ),
        borderWidth = 1,
        borderColor = "#ccc",
        padding = 10,
        textStyle = list(color = "#000"),
        backgroundColor = 'rgba(255, 255, 255, 0.8)',
        position = htmlwidgets::JS(
          "function (point, params, dom, rect, size) {
            return [point[0], '0%'];
          }"
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
      # legend = list(
      #   show = FALSE,
      #   orient = "vertical",
      #   bottom = "50%",
      #   right = "2%",
      #   # data = pft_unique
      # ),
      grid = list(
        list(left = "10%", right = "8%", top = "4%", height = "13%"),
        list(left = "10%", right = "8%", top = "20%", height = "13%"),
        list(left = "10%", right = "8%", top = "36%", height = "13%"),
        list(left = "10%", right = "8%", bottom = "35%", height = "13%"),
        list(left = "10%", right = "8%", bottom = "19%", height = "13%"),
        list(left = "10%", right = "8%", bottom = "3", height = "13%")
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
        ),
        list(
          type = "category",
          gridIndex = 5,
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
          type = "value",
          boundaryGap = FALSE,
          name = "Fraction",
          nameLocation = "middle",
          nameGap = 40,
          nameTextStyle = list(fontWeight = "bolder"),
          min = 0,
          max = 100,
          gridIndex = 0,
          scale = TRUE,
          splitArea = list(
            show = TRUE
          )
        ),
        list(
          name = "Precipitation[mm]",
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
          name = "Temperature[degC] & <br/>Temperature_Daylight[degC]",
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
          name = "PAR[µmolm-2s-1]",
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
          name = "Daylength[h]",
          nameLocation = "middle",
          nameGap = 40,
          nameTextStyle = list(fontWeight = "bolder"),
          scale = TRUE,
          gridIndex = 4,
          splitNumber = 5,
          min = 0,
          max = 24,
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
          name = "PET[mm]",
          nameLocation = "middle",
          nameGap = 40,
          nameTextStyle = list(fontWeight = "bolder"),
          scale = TRUE,
          gridIndex = 5,
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
      series = simulations
    )

  return(chart)
}

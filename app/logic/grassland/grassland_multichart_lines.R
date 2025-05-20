box::use(
  echarty[ec.init],
  htmlwidgets[JS],
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
          file_nr = (i - 1)
        )
      )
  }

  # Weather data ----
  weather_data <- read_weather_data(
    file_path = filepath_weather,
    end_date = grass_end_date,
    colors = colors_for_weather
  )

  simulations <- simulations |>
    append(weather_data)

  # Prepare time
  time <- get_time(filepaths_grass[1])

  # Echarty: making chart ----
  chart <- ec.init()
  chart$x$opts <-
    list(
      tooltip = list(
        trigger = "axis",
        formatter = JS(
          "
          function (param) {
            return '<strong>DATE: ' + param[0].name + '</strong><hr size=1 style=\"margin: 6px 0\">' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #73eb9b\"></i>File nr. 0 - PFT 0 - grasses - ' + param.find(item => item.seriesName ==  'PFT 0 file #0').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #e28bb7\"></i>File nr. 0 - PFT 1 - forbs - ' + param.find(item => item.seriesName ==  'PFT 1 file #0').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #998be2\"></i>File nr. 0 - PFT 2 - legumes - ' + param.find(item => item.seriesName ==  'PFT 2 file #0').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #73eb9b\"></i>File nr. 1 - PFT 0 -  grasses - ' + param.find(item => item.seriesName ==  'PFT 0 file #1').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #e28bb7\"></i>File nr. 1 - PFT 1 -  forbs - ' + param.find(item => item.seriesName ==  'PFT 1 file #1').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #998be2\"></i>File nr. 1 - PFT 2 -  legumes - ' + param.find(item => item.seriesName ==  'PFT 2 file #1').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #73eb9b\"></i>File nr. 2 - PFT 0 -  grasses - ' + param.find(item => item.seriesName ==  'PFT 0 file #2').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #e28bb7\"></i>File nr. 2 - PFT 1 -  forbs - ' + param.find(item => item.seriesName ==  'PFT 1 file #2').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #998be2\"></i>File nr. 2 - PFT 2 -  legumes - ' + param.find(item => item.seriesName ==  'PFT 2 file #2').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #73eb9b\"></i>File nr. 3 - PFT 0 -  grasses - ' + param.find(item => item.seriesName ==  'PFT 0 file #3').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #e28bb7\"></i>File nr. 3 - PFT 1 -  forbs - ' + param.find(item => item.seriesName ==  'PFT 1 file #3').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #998be2\"></i>File nr. 3 - PFT 2 -  legumes - ' + param.find(item => item.seriesName ==  'PFT 2 file #3').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #73eb9b\"></i>File nr. 4 - PFT 0 -  grasses - ' + param.find(item => item.seriesName ==  'PFT 0 file #4').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #e28bb7\"></i>File nr. 4 - PFT 1 -  forbs - ' + param.find(item => item.seriesName ==  'PFT 1 file #4').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #998be2\"></i>File nr. 4 - PFT 2 -  legumes - ' + param.find(item => item.seriesName ==  'PFT 2 file #4').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #73eb9b\"></i>File nr. 5 - PFT 0 -  grasses - ' + param.find(item => item.seriesName ==  'PFT 0 file #5').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #e28bb7\"></i>File nr. 5 - PFT 1 -  forbs - ' + param.find(item => item.seriesName ==  'PFT 1 file #5').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #998be2\"></i>File nr. 5 - PFT 2 -  legumes - ' + param.find(item => item.seriesName ==  'PFT 2 file #5').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #73eb9b\"></i>File nr. 6 - PFT 0 -  grasses - ' + param.find(item => item.seriesName ==  'PFT 0 file #6').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #e28bb7\"></i>File nr. 6 - PFT 1 -  forbs - ' + param.find(item => item.seriesName ==  'PFT 1 file #6').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #998be2\"></i>File nr. 6 - PFT 2 -  legumes - ' + param.find(item => item.seriesName ==  'PFT 2 file #6').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #73eb9b\"></i>File nr. 7 - PFT 0 -  grasses - ' + param.find(item => item.seriesName ==  'PFT 0 file #7').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #e28bb7\"></i>File nr. 7 - PFT 1 -  forbs - ' + param.find(item => item.seriesName ==  'PFT 1 file #7').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #998be2\"></i>File nr. 7 - PFT 2 -  legumes - ' + param.find(item => item.seriesName ==  'PFT 2 file #7').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #73eb9b\"></i>File nr. 8 - PFT 0 -  grasses - ' + param.find(item => item.seriesName ==  'PFT 0 file #8').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #e28bb7\"></i>File nr. 8 - PFT 1 -  forbs - ' + param.find(item => item.seriesName ==  'PFT 1 file #8').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #998be2\"></i>File nr. 8 - PFT 2 -  legumes - ' + param.find(item => item.seriesName ==  'PFT 2 file #8').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #73eb9b\"></i>File nr. 9 - PFT 0 -  grasses - ' + param.find(item => item.seriesName ==  'PFT 0 file #9').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #e28bb7\"></i>File nr. 9 - PFT 1 -  forbs - ' + param.find(item => item.seriesName ==  'PFT 1 file #9').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #998be2\"></i>File nr. 9 - PFT 2 -  legumes - ' + param.find(item => item.seriesName ==  'PFT 2 file #9').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #73eb9b\"></i>File nr. 10 - PFT 0 -  grasses - ' + param.find(item => item.seriesName ==  'PFT 0 file #10').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #e28bb7\"></i>File nr. 10 - PFT 1 -  forbs - ' + param.find(item => item.seriesName ==  'PFT 1 file #10').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #998be2\"></i>File nr. 10 - PFT 2 -  legumes - ' + param.find(item => item.seriesName ==  'PFT 2 file #10').value +
              '<hr size=1 style=\"margin: 4px 0\">' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #0072B2\"></i>Precipitation [mm]: ' + param.find(item => item.seriesName ==  'Precipitation[mm]').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #D55E00\"></i>Temperature [degC]: ' + param.find(item => item.seriesName ==  'Temperature[degC]').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #E69F00\"></i>Temperature Daylight [degC]: ' + param.find(item => item.seriesName ==  'Temperature_Daylight[degC]').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #F0E442\"></i>PAR [µmolm-2s-1]: ' + param.find(item => item.seriesName ==  'PAR[µmolm-2s-1]').value + '<br />' +
              '<i class=\"fa fa-circle\" aria-hidden=\"true\" style=\"color: #009E73\"></i>Daylength [h]: ' + param.find(item => item.seriesName ==  'Daylength[h]').value + '<br />' +
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
        list(left = "10%", right = "8%", top = "4%", height = "13%"),
        list(left = "10%", right = "8%", top = "22%", height = "13%"),
        list(left = "10%", right = "8%", top = "38%", height = "13%"),
        list(left = "10%", right = "8%", bottom = "34%", height = "13%"),
        list(left = "10%", right = "8%", bottom = "18%", height = "13%"),
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
          name = "Daylength [h]",
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
          name = "PET [mm]",
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

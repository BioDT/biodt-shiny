box::use(
  readr[read_csv, cols, read_file],
  dplyr[filter, rename, mutate, left_join, select],
  echarty[ec.init, ec.data],
  stringr[str_split],
  stats[na.omit],
)

#' @export
honeybee_beekeeper_plot <- function(input_filepath,
                                    weather_filepath) {
  input <- read_csv(input_filepath,
    col_types = cols()
  ) |>
    filter(
      `[run number]` == 1,
      siminputrow == 1
    ) |>
    rename(
      Step = `[step]`,
      `Honey (kg)` = `(honeyEnergyStore / ( ENERGY_HONEY_per_g * 1000 ))`
    ) |>
    mutate(
      Step2 = Step %% 365,
      `Honey (kg)` = round(`Honey (kg)`, 2)
    )

  weather <- read_file(weather_filepath) |>
    str_split(" ",
      simplify = TRUE
    ) |>
    as.integer() |>
    na.omit()

  weather_data <-
    data.frame(
      weather = weather,
      Step2 = 0:(length(weather) - 1)
    )

  input <- input |>
    left_join(weather_data,
      by = "Step2"
    )

  echarty_plot <- echarty::ec.init(
    preset = FALSE,
    xAxis = list(
      type = "value",
      name = "Step",
      nameGap = 0,
      nameLocation = "center",
      nameTextStyle = list(
        align = "center",
        verticalAlign = "top",
        padding = list(30, 0, 0, 0),
        fontSize = 15
      )
    ),
    yAxis = list(
      list(
        type = "value",
        min = 0,
        max = max(input$`Honey (kg)`) + 5,
        name = "Honey (kg)",
        nameTextStyle = list(fontSize = 15)
      ),
      list(
        type = "value",
        min = 0,
        max = 24,
        show = FALSE,
        name = "Day hours for collecting"
      ),
      list(
        type = "value",
        min = 0,
        max = max(input$`TotalIHbees + TotalForagers` + 100),
        name = "Bees count",
        nameTextStyle = list(fontSize = 15)
      )
    ),
    series = list(
      list(
        type = "bar",
        data = echarty::ec.data(input |> select(
          Step,
          weather
        )),
        yAxisIndex = 2,
        color = "#009E73",
        itemStyle = list(opacity = 0.3),
        barWidth = "100%",
        name = "Collection hours"
      ),
      list(
        type = "line",
        showSymbol = FALSE,
        name = "Honey (kg)",
        lineStyle = list(width = 3),
        color = "#E69F00",
        data = echarty::ec.data(
          input |> select(
            Step,
            `Honey (kg)`
          )
        )
      ),
      list(
        type = "line",
        showSymbol = FALSE,
        data = echarty::ec.data(input |> select(
          Step,
          `TotalIHbees + TotalForagers`
        )),
        yAxisIndex = 3,
        lineStyle = list(width = 3),
        color = "#0072B2",
        name = "Bees Count"
      )
    ),
    tooltip = list(
      show = TRUE,
      trigger = "axis"
    )
  )

  return(echarty_plot)
}

box::use(
  readr[read_csv, cols, read_file],
  dplyr[filter, rename, mutate, left_join, select],
  echarty[ec.init, ec.data],
  stringr[str_split],
  stats[na.omit],
)

#' @export
read_plot_data <- function(input_filepath) {
  print("reading data")
  input <- read_csv(input_filepath,
    col_types = cols()
  ) |>
    rename(
      `Honey (kg)` = `(honeyEnergyStore / ( ENERGY_HONEY_per_g * 1000 ))`,
      Date = date
    ) |>
    mutate(
      `Honey (kg)` = round(`Honey (kg)`, 2)
    )
}


#' @export
honeybee_beekeeper_plot <- function(input_filepath = NULL,
                                    input = NULL, i18n = i18n) {
  if (is.null(input)) {
    print("reading data")
    input <- read_csv(input_filepath,
      col_types = cols()
    ) |>
      rename(
        `Honey (kg)` = `(honeyEnergyStore / ( ENERGY_HONEY_per_g * 1000 ))`,
        Date = date
      ) |>
      mutate(
        `Honey (kg)` = round(`Honey (kg)`, 2)
      )
  }

  echarty_plot <- ec.init(
    preset = FALSE,
    xAxis = list(
      type = "category",
      name = i18n$t("Date"),
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
        name = i18n$t("Honey (kg)"),
        nameTextStyle = list(fontSize = 15)
      ),
      list(
        type = "value",
        min = 0,
        max = 24,
        show = FALSE,
        name = i18n$t("Day hours for collecting")
      ),
      list(
        type = "value",
        min = 0,
        max = max(input$`TotalIHbees + TotalForagers` + 100),
        name = i18n$t("Bees count"),
        nameTextStyle = list(fontSize = 15)
      )
    ),
    series = list(
      list(
        type = "bar",
        data = ec.data(input |> select(
          Date,
          weather
        )),
        yAxisIndex = 2,
        color = "#009E73",
        itemStyle = list(opacity = 0.3),
        barWidth = "100%",
        name = i18n$t("Collection hours")
      ),
      list(
        type = "line",
        showSymbol = FALSE,
        name = i18n$t("Honey (kg)"),
        lineStyle = list(width = 3),
        color = "#E69F00",
        data = ec.data(
          input |> select(
            Date,
            `Honey (kg)`
          )
        )
      ),
      list(
        type = "line",
        showSymbol = FALSE,
        data = ec.data(input |> select(
          Date,
          `TotalIHbees + TotalForagers`
        )),
        yAxisIndex = 3,
        lineStyle = list(width = 3),
        color = "#0072B2",
        name = i18n$t("Bees Count")
      )
    ),
    tooltip = list(
      show = TRUE,
      trigger = "axis"
    )
  )

  return(echarty_plot)
}

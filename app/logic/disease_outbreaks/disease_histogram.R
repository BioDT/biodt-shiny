box::use(
  echarty[ec.init],
)

#' @export
disease_histogram <- function(
    hist_data,
    i18n) {
  series_list <- list()
  for (column in names(hist_data)) {
    series_list[[length(series_list) + 1]] <- list(
      name = column,
      type = "bar",
      stack = column,
      data = hist_data[[column]]
    )
  }
  chart <- ec.init()
  chart$x$opts <-
    list(
      title = list(text = i18n$t("Secondary infection data")),
      tooltip = list(
        trigger = "axis"
      ),
      legend = list(data = names(hist_data)),
      xAxis = list(
        type = "category",
        boundaryGap = TRUE,
        name = i18n$t("Number of Secondary Infections"),
        nameLocation = "middle",
        nameGap = 40,
        nameTextStyle = list(fontWeight = "bolder"),
        data = seq_along(hist_data[[1]])
      ),
      yAxis = list(
        type = "value",
        boundaryGap = FALSE,
        name = i18n$t("Frequency"),
        nameLocation = "middle",
        nameGap = 25,
        nameTextStyle = list(fontWeight = "bolder"),
        min = 0,
        max = max(hist_data, 0, na.rm = TRUE)
      ),
      series = series_list
    )
  return(chart)
}

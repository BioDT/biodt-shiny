box::use(
  ggplot2[ggplot, aes, geom_line, geom_point, labs, theme_bw, scale_y_continuous, scale_x_date],
  patchwork,
  dplyr[mutate, select, across, arrange, n, summarize, rowwise, ungroup], 
  lubridate[as_date],
  scales[label_number, cut_short_scale],
  rlang[sym]
)

#' Create Summary Statistics Plots
#'
#' Generates a multi-panel plot showing cumulative records, daily records,
#' and species counts over time.
#'
#' @param summary_data A data frame containing summary statistics in wide format:
#'   'date' column and subsequent columns for each species count.
#' @return A patchwork/ggplot object (combined plot).
#' @export
create_summary_plots <- function(summary_data) {

  # Ensure date is in Date format and data is valid
  if (is.null(summary_data) || nrow(summary_data) == 0 || ncol(summary_data) < 2) {
    return(
      ggplot() +
        labs(title = "No summary data available or data format incorrect") +
        theme_bw()
    )
  }

  # --- Data Transformation --- #
  # Calculate summaries from the wide format data
  plot_data <- summary_data |>
    mutate(date = as_date(date)) |>
    arrange(date) |>
    # Calculate daily total records (sum across species columns)
    rowwise() |> # Process row by row for sums/counts across columns
    mutate(
      daily_record = sum(across(-date), na.rm = TRUE),
      # Calculate daily species count (count species columns > 0)
      species_count = sum(across(-date) > 0, na.rm = TRUE)
    ) |> 
    ungroup() |> # Ungroup after rowwise operations
    # Calculate cumulative records
    mutate(cumulative_record = cumsum(daily_record)) |>
    # Select only the columns needed for plotting
    select(date, cumulative_record, daily_record, species_count)
  # --- End Data Transformation --- #


  # Common theme elements
  common_theme <- theme_bw()
  common_line_point <- list(
    geom_line(color = "brown"),
    geom_point(color = "brown")
  )
  common_x_scale <- scale_x_date(date_breaks = "2 days", date_labels = "%b %d")

  # Plot 1: Cumulative Record
  p1 <- ggplot(plot_data, aes(x = date, y = cumulative_record)) +
    common_line_point +
    common_x_scale +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    labs(
      title = "Cumulative record",
      x = NULL,
      y = "Recordings / Count"
    ) +
    common_theme

  # Plot 2: Daily Record
  p2 <- ggplot(plot_data, aes(x = date, y = daily_record)) +
    common_line_point +
    common_x_scale +
    scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
    labs(
      title = "Daily record",
      x = "Date",
      y = NULL
    ) +
    common_theme

  # Plot 3: Species Count
  p3 <- ggplot(plot_data, aes(x = date, y = species_count)) +
    common_line_point +
    common_x_scale +
    labs(
      title = "Species",
      x = NULL,
      y = NULL
    ) +
    common_theme

  # Combine plots using patchwork
  combined_plot <- p1 + p2 + p3

  return(combined_plot)
}

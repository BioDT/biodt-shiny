box::use(
  ggplot2[ggplot, aes, geom_line, geom_point, labs, theme_bw, scale_y_continuous, scale_x_date, scale_y_reverse, scale_color_discrete, theme, element_blank],
  patchwork,
  dplyr[mutate, select, across, arrange, n, summarize, rowwise, ungroup, group_by, filter, desc, dense_rank, left_join, rename],
  lubridate[as_date],
  scales[label_number, cut_short_scale],
  rlang[sym],
  tidyr[pivot_longer],
  utils[head]
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

#' Create Top 5 Species Rank Plot
#'
#' Generates a plot showing the rank (1-5) of the most recorded species per day.
#'
#' @param summary_data A data frame containing summary statistics in wide format:
#'   'date' column and subsequent columns for each species count.
#' @return A ggplot object.
#' @export
create_top_species_rank_plot <- function(summary_data) {
  # Ensure data is valid
  if (is.null(summary_data) || nrow(summary_data) == 0 || ncol(summary_data) < 2) {
    return(
      ggplot() +
        labs(title = "No summary data available or data format incorrect") +
        theme_bw()
    )
  }

  # --- Data Transformation for Rank Plot --- #
  rank_data <- summary_data |>
    mutate(date = as_date(date)) |>
    # Pivot to long format: date, species, count
    pivot_longer(cols = -date, names_to = "species", values_to = "count") |>
    # Remove rows where count is 0 or NA, as they can't be ranked
    filter(count > 0 & !is.na(count)) |>
    group_by(date) |>
    # Rank species within each date based on count (descending)
    # dense_rank ensures consecutive ranks even with ties
    mutate(rank = dense_rank(desc(count))) |>
    # Keep only the top 5 ranks
    filter(rank <= 5) |>
    ungroup()
  # --- End Data Transformation --- #

  # Check if we have data after filtering
  if (nrow(rank_data) == 0) {
    return(
      ggplot() +
        labs(title = "Not enough data to determine top 5 species ranks") +
        theme_bw()
    )
  }

  # --- Create Plot --- #
  p_rank <- ggplot(rank_data, aes(x = date, y = rank, color = species)) +
    geom_line(linewidth = 1) + # Use linewidth instead of size
    geom_point(size = 3) +
    scale_y_reverse(breaks = 1:5) + # Reverse Y axis, show breaks 1 to 5
    scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
    scale_color_discrete(name = "Bird species") + # Legend title
    labs(
      title = "Top 5 most recorded species per day",
      x = "Date",
      y = "Count rank number"
    ) +
    theme_bw() +
    theme(legend.position = "bottom") # Place legend at the bottom

  return(p_rank)
}

#' Create Data for Top 5 Species Table (Figure 5)
#'
#' Generates a dataframe showing the details of the top 5 most recorded species per day.
#'
#' @param summary_data A data frame containing summary statistics in wide format:
#'   'date' column and subsequent columns for each species count (using scientific names).
#' @param bird_spp_info A dataframe mapping scientific names to vernacular names.
#'   Expected columns: 'scientific_name', 'vernacular_name'.
#' @return A dataframe suitable for DT::datatable.
#' @export
create_top_species_table_data <- function(summary_data, bird_spp_info) {
  # Ensure data is valid
  if (is.null(summary_data) || nrow(summary_data) == 0 || ncol(summary_data) < 2) {
    return(data.frame(Message = "No summary data available or data format incorrect"))
  }
  # Check for 'scientific_name' and 'common_name'
  if (is.null(bird_spp_info) || !all(c("scientific_name", "common_name") %in% names(bird_spp_info))) {
    return(data.frame(Message = "Bird species information is missing required columns (scientific_name, common_name)"))
  }

  # --- Data Transformation for Table --- #
  top5_data_before_join <- summary_data |>
    mutate(date = as_date(date)) |>
    # Pivot to long format: date, scientific_name_col, count
    pivot_longer(cols = -date, names_to = "scientific_name_col", values_to = "count") |>
    # Remove rows where count is 0 or NA, as they can't be ranked
    filter(count > 0 & !is.na(count)) |>
    group_by(date) |>
    # Rank species within each date based on count (descending)
    mutate(rank = dense_rank(desc(count))) |>
    # Keep only the top 5 ranks
    filter(rank <= 5) |>
    ungroup()

  # --- Debug: Before Join --- #
  print("Data before join (head):")
  print(head(top5_data_before_join))
  # --- End Debug --- #

  top5_data_after_join <- top5_data_before_join |>
    # Join with species info to get vernacular name
    left_join(bird_spp_info, by = c("scientific_name_col" = "scientific_name"))

  # --- Debug: After Join --- #
  print("Data after join (head):")
  print(head(top5_data_after_join))
  # --- End Debug --- #

  top5_data <- top5_data_after_join |>
    # Select and rename columns
    select(
      Date = date,
      `Vernacular name` = common_name, # Use common_name, rename column
      `Scientific name` = scientific_name_col, # Use backticks for space
      Count = count,
      rank # Keep rank for sorting
    ) |>
    # Arrange by date, then by rank
    arrange(Date, rank) |>
    select(-rank) # Remove rank column after sorting
  # --- End Data Transformation --- #

  if (nrow(top5_data) == 0) {
    return(data.frame(Message = "Not enough data to determine top 5 species"))
  }

  return(top5_data)
}

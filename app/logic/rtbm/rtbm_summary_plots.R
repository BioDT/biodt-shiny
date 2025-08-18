box::use(
  ggplot2[ggplot, aes, geom_line, geom_point, labs, theme_bw, scale_y_continuous, scale_x_date, scale_y_reverse, scale_color_discrete, theme, element_blank],
  patchwork[plot_layout],
  dplyr[mutate, select, across, arrange, n, summarize, rowwise, ungroup, group_by, filter, desc, dense_rank, left_join, rename],
  lubridate[as_date, days],
  scales[label_number, cut_short_scale],
  rlang[sym],
  tidyr[pivot_longer],
  utils[head],
  stringr[str_replace_all, str_to_title],
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
create_summary_plots <- function(summary_data, i18n) {
  # Ensure date is in Date format and data is valid
  if (is.null(summary_data) || nrow(summary_data) == 0 || ncol(summary_data) < 2) {
    return(
      ggplot() +
        labs(title = i18n$t("No summary data available or data format incorrect")) +
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
  # Always stack plots vertically (one plot per row)
  combined_plot <- p1 / p2 / p3

  return(combined_plot)
}

#' Create Top 5 Species Rank Plot
#'
#' Generates a plot showing the rank (1-5) of the most recorded species per day.
#'
#' @param summary_data A data frame containing summary statistics in wide format:
#'   'date' column and subsequent columns for each species count.
#' @param bird_spp_info A dataframe mapping scientific names to vernacular names.
#'   Expected columns: 'join_key_scientific_name', 'common_name'.
#' @return A ggplot object.
#' @export
create_top_species_rank_plot <- function(summary_data, bird_spp_info) {
  # Ensure data is valid
  if (is.null(summary_data) || nrow(summary_data) == 0 || ncol(summary_data) < 2) {
    return(
      ggplot() +
        labs(title = "No summary data available or data format incorrect") +
        theme_bw()
    )
  }
  # Check for 'join_key_scientific_name' and 'common_name' in bird_spp_info
  if (is.null(bird_spp_info) || !all(c("join_key_scientific_name", "common_name") %in% names(bird_spp_info))) {
    return(
      ggplot() +
        labs(title = "Bird species information is missing required columns (join_key_scientific_name, common_name)") +
        theme_bw()
    )
  }

  # --- Data Transformation for Rank Plot --- #
  rank_data_intermediate <- summary_data |>
    mutate(date = as_date(date)) |>
    # Pivot to long format: date, species_scientific, count
    pivot_longer(cols = -date, names_to = "species_scientific", values_to = "count") |>
    # Remove rows where count is 0 or NA, as they can't be ranked
    filter(count > 0 & !is.na(count)) |>
    group_by(date) |>
    # Rank species within each date based on count (descending)
    mutate(rank = dense_rank(desc(count))) |>
    # Keep only the top 5 ranks
    filter(rank <= 5) |>
    ungroup()

  # Join with bird_spp_info to get common names
  # Ensure the scientific name format matches for joining (e.g. "Genus_species")
  rank_data <- rank_data_intermediate |>
    mutate(species_scientific_join_key = str_replace_all(species_scientific, " ", "_")) |>
    left_join(
      select(bird_spp_info, join_key_scientific_name, common_name),
      by = c("species_scientific_join_key" = "join_key_scientific_name")
    ) |>
    # Handle cases where a common name might be NA after the join (though ideally shouldn't happen with good data)
    mutate(common_name = ifelse(is.na(common_name), species_scientific, common_name))
  # --- End Data Transformation --- #

  # Check if we have data after filtering and joining
  if (nrow(rank_data) == 0) {
    return(
      ggplot() +
        labs(title = "Not enough data to determine top 5 species ranks") +
        theme_bw()
    )
  }

  # --- Create Plot --- #
  p_rank <- ggplot(rank_data, aes(x = date, y = rank, color = common_name)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    scale_y_reverse(breaks = 1:5) +
    scale_x_date(date_breaks = "2 days", date_labels = "%b %d") +
    scale_color_discrete(name = "Bird Species") +
    labs(
      title = "Top 5 most recorded species per day",
      x = "Date",
      y = "Count rank number"
    ) +
    theme_bw() +
    theme(legend.position = "bottom")

  return(p_rank)
}

#' Create Data for Top 5 Species Table (Figure 5)
#'
#' Generates a dataframe showing the details of the top 5 most recorded species per day.
#'
#' @param summary_data A data frame containing summary statistics in wide format:
#'   'date' column and subsequent columns for each species count (using scientific names).
#' @param bird_spp_info A dataframe mapping scientific names to vernacular names.
#'   Expected columns: 'join_key_scientific_name', 'display_scientific_name', 'common_name'.
#' @return A dataframe suitable for DT::datatable.
#' @export
create_top_species_table_data <- function(summary_data, bird_spp_info) {
  # Ensure data is valid
  if (is.null(summary_data) || nrow(summary_data) == 0 || ncol(summary_data) < 2) {
    return(data.frame(Message = "No summary data available or data format incorrect"))
  }
  # Check for 'join_key_scientific_name', 'display_scientific_name', and 'common_name'
  if (is.null(bird_spp_info) || !all(c("join_key_scientific_name", "display_scientific_name", "common_name") %in% names(bird_spp_info))) {
    return(data.frame(Message = "Bird species information is missing required columns (join_key_scientific_name, display_scientific_name, common_name)"))
  }

  # --- Data Transformation for Table --- #
  top5_data_before_join <- summary_data |>
    mutate(date = as_date(date)) |>
    # Pivot to long format: date, name_col, count
    pivot_longer(cols = -date, names_to = "name_col", values_to = "count") |>
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
    mutate(name_col = str_replace_all(name_col, " ", "_")) |>
    left_join(bird_spp_info, by = c("name_col" = "join_key_scientific_name"))

  # --- Debug: After Join --- #
  print("Data after join (head):")
  print(head(top5_data_after_join))
  # --- End Debug --- #

  top5_data <- top5_data_after_join |>
    # Format the name_col for display
    mutate(name_col = str_replace_all(name_col, "_", " ")) |>
    mutate(name_col = str_to_title(name_col)) |>
    # Format display_scientific_name for italics and spaces
    mutate(display_scientific_name = paste0("<em>", str_replace_all(display_scientific_name, "_", " "), "</em>")) |>
    # Select and rename columns
    select(
      Date = date,
      # Name = name_col, # Column hidden as per user request
      `Common name` = common_name,
      `Scientific name` = display_scientific_name,
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

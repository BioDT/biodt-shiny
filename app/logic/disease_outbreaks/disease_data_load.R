box::use(
  shiny,
  readr[
    read_csv
  ],
  terra,
)

#' @export
load_simulated_data <- function(
    run_dir,
    r_disease,
    i18n) {
  # Find all CSV files for grid data
  r_disease$infected_files <- list.files(
    file.path(run_dir, "epi_stat_outputs"),
    pattern = "^epi_stat_infected_tick_\\d+\\.csv$",
    full.names = TRUE
  )
  r_disease$resistant_files <- list.files(
    file.path(run_dir, "epi_stat_outputs"),
    pattern = "^epi_stat_resistant_tick_\\d+\\.csv$",
    full.names = TRUE
  )
  r_disease$susceptible_files <- list.files(
    file.path(run_dir, "epi_stat_outputs"),
    pattern = "^epi_stat_susceptible_tick_\\d+\\.csv$",
    full.names = TRUE
  )

  # Extract tick numbers
  r_disease$infected_ticks <- as.numeric(gsub(".*_tick_(\\d+)\\.csv", "\\1", r_disease$infected_files))
  r_disease$resistant_ticks <- as.numeric(gsub(".*_tick_(\\d+)\\.csv", "\\1", r_disease$resistant_files))
  r_disease$susceptible_ticks <- as.numeric(gsub(".*_tick_(\\d+)\\.csv", "\\1", r_disease$susceptible_files))

  # Find common ticks across all grid types
  r_disease$available_ticks <- sort(Reduce(
    intersect,
    list(r_disease$infected_ticks, r_disease$resistant_ticks, r_disease$susceptible_ticks)
  ))

  ext_run <- terra::ext(
    r_disease$bounds[1],
    r_disease$bounds[3],
    r_disease$bounds[2],
    r_disease$bounds[4]
  ) |>
    terra::project(
      from = "EPSG:3035",
      to = "EPSG:4326"
      # to = "EPSG:3857"
    )

  r_disease$infected_data <- list()
  r_disease$resistant_data <- list()
  r_disease$susceptible_data <- list()

  for (i in r_disease$available_ticks) {
    r_disease$infected_data[[as.character(i)]] <- as.matrix(
      read_csv(
        r_disease$infected_files[match(
          i,
          r_disease$infected_ticks
        )],
        show_col_types = FALSE,
        col_names = FALSE
      )
    ) |>
      terra$rast(
        extent = ext_run,
        crs = "EPSG:4326"
      )

    r_disease$resistant_data[[as.character(i)]] <- as.matrix(
      read_csv(
        r_disease$resistant_files[match(
          i,
          r_disease$resistant_ticks
        )],
        show_col_types = FALSE,
        col_names = FALSE
      )
    ) |>
      terra$rast(
        extent = ext_run,
        crs = "EPSG:4326"
      )

    r_disease$susceptible_data[[as.character(i)]] <- as.matrix(
      read_csv(
        r_disease$susceptible_files[match(
          i,
          r_disease$susceptible_ticks
        )],
        show_col_types = FALSE,
        col_names = FALSE
      )
    ) |>
      terra$rast(
        extent = ext_run,
        crs = "EPSG:4326"
      )
  }

  shiny$showNotification(
    i18n$t("Results loaded"),
    type = "message"
  )

  # Find all secondary infection CSV files
  r_disease$sec_inf_files <- list.files(
    file.path(run_dir, "sec_inf_outputs"),
    pattern = "^secondary_infections_tick_\\d+\\.csv$",
    full.names = TRUE
  )

  # Extract tick numbers for secondary infection files
  r_disease$sec_inf_ticks <- as.numeric(gsub(".*_tick_(\\d+)\\.csv", "\\1", r_disease$sec_inf_files)) |>
    sort()

  # If no secondary infection ticks found, show error
  if (length(r_disease$sec_inf_ticks) == 0) {
    shiny$showNotification(i18n$t("No secondary infection data found"), type = "error")
  }

  # Initialize list to store secondary infection data
  r_disease$sec_inf_data <- vector("list", length(r_disease$sec_inf_ticks))
  names(r_disease$sec_inf_data) <- as.character(r_disease$sec_inf_ticks)

  # Load secondary infection data
  for (i in r_disease$sec_inf_ticks) {
    r_disease$sec_inf_data[[as.character(i)]] <- read_csv(
      r_disease$sec_inf_files[match(i, r_disease$sec_inf_ticks)],
      show_col_types = FALSE,
      col_names = TRUE
    )
  }

  return(r_disease)
}

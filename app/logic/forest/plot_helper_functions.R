# Load the libraries
box::use(
  dplyr,
  terra,
  leaflet,
  stringr,
  utils,
  shiny,
  echarty,
  app /
    logic /
    forest /
    landis_io[read_landis_params]
)

#' @export
plot_bird_species <- function(scenario,
                              bird_species,
                              tick,
                              prediction_folder,
                              i18n) {
  prediction_file <- file.path(prediction_folder, scenario, tick, paste0(bird_species, ".tif"))
  if (file.exists(prediction_file)) {
    species_rast <- terra$rast(prediction_file)
    leaflet$leafletProxy("map") |>
      leaflet$removeImage("bird_species") |>
      leaflet$addRasterImage(
        species_rast,
        opacity = 0.3,
        colors = "viridis",
        project = FALSE,
        layerId = "bird_species",
        group = "bird_species",
        options = leaflet$tileOptions(zIndex = 2)
      )
  } else {
    leaflet$leafletProxy("map") |>
      leaflet$removeImage("bird_species")
    shiny$showNotification(i18n$t("Warning: Bird species file does not exist!"), type = "error")
  }
}

#' @export
plot_tree_species <- function(data_folder, res_file, i18n) {
  simulation_file <- file.path(data_folder, res_file)
  
  if (file.exists(simulation_file)) {
    raster_data <- terra$rast(simulation_file)

    # Ensure there are any values at all (handles multi-layer rasters too)
    if (!any(terra$hasValues(raster_data))) {
      leaflet$leafletProxy("map") |>
        leaflet$removeImage("tree_species") |>
        leaflet$removeControl("tree_legend")
      shiny$showNotification("Warning: Tree species raster has no data (all NA).", type = "error")
      return(invisible(NULL))
    }

    # Robust global min/max across all layers
    mm <- terra$minmax(raster_data)               # 2 x nlyr matrix
    rmin <- suppressWarnings(min(mm[1, ], na.rm = TRUE))
    rmax <- suppressWarnings(max(mm[2, ], na.rm = TRUE))

    # Validate range
    if (!is.finite(rmin) || !is.finite(rmax)) {
      leaflet$leafletProxy("map") |>
        leaflet$removeImage("tree_species") |>
        leaflet$removeControl("tree_legend")
      shiny$showNotification(i18n$t("Warning: Raster contains non-finite range!"), type = "error")
      return(invisible(NULL))
    }
    # Avoid zero-width domain (flat rasters)
    if (rmin == rmax) {
      eps  <- if (rmin == 0) 1 else abs(rmin) * 0.01
      rmin <- rmin - eps
      rmax <- rmax + eps
    }

    pal_base <- leaflet$colorNumeric(
      palette  = "YlOrBr",
      domain   = c(rmin, rmax),
      na.color = "transparent",
      reverse  = TRUE
    )
    pal <- function(x) pal_base(pmin(pmax(x, rmin), rmax))

    leaflet$leafletProxy("map") |>
      leaflet$removeImage("tree_species") |>
      leaflet$removeControl("tree_legend") |>
      leaflet$addRasterImage(
        raster_data,
        opacity = 0.4,
        colors = pal,
        project = FALSE,
        layerId = "tree_species",
        group = "tree_species",
        options = leaflet$tileOptions(zIndex = 1)
      ) |>
      leaflet$addLegend(
        position = "bottomright",
        pal = pal_base,
        values = c(rmin, rmax),
        opacity = 0.4,
        layerId = "tree_legend"
      )

  } else {
    leaflet$leafletProxy("map") |>
      leaflet$removeImage("tree_species") |>
      leaflet$removeControl("tree_legend")
    shiny$showNotification(i18n$t("Warning: Tree species file does not exist!"), type = "error")
  }
}

#' @export
make_x_axes <- function(df) {
  # Build xAxis for each of the 28 grids independently, using the
  # management column of the grid to select an appropriate time vector.
  # Fallback to unique sorted Time if a specific filter yields empty data.
  mgmt <- c("BAU", "EXT10", "EXT30", "GTR30", "NTLR", "NTSR", "SA")
  lapply(0:27, function(i) {
    col <- i %% 7
    # prefer climate 4.5; if missing, drop climate filter
    tv <- df[df$Management == mgmt[col + 1] & df$Climate == "4.5", "Time"]
    if (length(tv) == 0) {
      tv <- df[df$Management == mgmt[col + 1], "Time"]
    }
    if (length(tv) == 0) {
      tv <- sort(unique(df$Time))
    }
    # enforce numeric ascending order then to character
    tv <- sort(unique(as.numeric(tv)))
    tv <- as.character(tv)
    ax <- list(type = "category", data = tv, gridIndex = i)
    if (i >= 21) { # bottom row needs an axis title
      ax$name <- "year"
      ax$nameLocation <- "middle"
      ax$nameGap <- 30
      ax$nameTextStyle <- list(
        fontSize = 13,
        align = "center",
        color = "black"
      )
    }
    ax
  })
}

#' @export
make_y_axes <- function() {
  name_map <- c(
    "AGBiomass\n(g/m^2)", rep(NA, 6),
    "BGBiomass\n(g/m^2)", rep(NA, 6),
    "Age\n(years)", rep(NA, 6),
    "Woody Debris\n(kgDW/m^2)", rep(NA, 6)
  )

  lapply(0:27, function(i) {
    ax <- list(type = "value", gridIndex = i)
    if (!is.na(name_map[i + 1])) {
      ax$name <- name_map[i + 1]
      ax$nameLocation <- "middle"
      ax$nameGap <- 50
      ax$nameTextStyle <- list(
        fontSize = 12,
        align = "center",
        color = "black"
      )
    }
    ax
  })
}

#' @export
make_grids <- function() {
  # lefts <- c('4%', '18%', '32%', '46%', '60%', '74%', '88%')
  lefts <- c("7%", "21%", "35%", "49%", "63%", "77%", "91%")
  tops <- c("10%", "31%", "56%", "81%")
  grids <- list()
  for (t in tops) {
    for (l in lefts) {
      grids[[length(grids) + 1L]] <- list(
        left = l,
        top = t,
        width = "8%",
        height = "12%"
      )
    }
  }
  grids
}

# helper: row labels on the left side of the chart
make_row_titles <- function() {
  list(
    list(left = "1%", top = "16%", text = "AGBiomass\n(g/m^2)", textStyle = list(fontSize = 13)),
    list(left = "1%", top = "41%", text = "BGBiomass\n(g/m^2)", textStyle = list(fontSize = 13)),
    list(left = "1%", top = "66%", text = "Age\n(years)", textStyle = list(fontSize = 13)),
    list(left = "1%", top = "91%", text = "Woody Debris\n(kgDW/m^2)", textStyle = list(fontSize = 13))
  )
}

#' @export
make_series <- function(df) {
  mgmt <- c("BAU", "EXT10", "EXT30", "GTR30", "NTLR", "NTSR", "SA")
  climate <- c("4.5", "8.5", "current")
  variables <- c(
    "AverageB.g.m2.",
    "AverageBelowGround.g.m2.",
    "AverageAge",
    "WoodyDebris.kgDW.m2."
  )
  climate_col <- c("4.5" = "red", "8.5" = "green", "current" = "blue")

  series <- list()
  # map each series to the correct grid (4 rows x 7 cols => 28 grids, indices 0..27)
  # rows correspond to variables; columns correspond to management regimes
  for (var in variables) {
    row <- match(var, variables) - 1L # 0..3
    for (m in mgmt) {
      col <- match(m, mgmt) - 1L # 0..6
      grid_index <- row * 7L + col # 0..27
      for (cl in climate) {
        # order series by Time ascending to match xAxis order
        df_sub <- df[df$Management == m & df$Climate == cl, c("Time", var)]
        if (nrow(df_sub) > 0) {
          ord <- order(as.numeric(df_sub$Time))
          yvals <- df_sub[[var]][ord]
        } else {
          yvals <- numeric(0)
        }
        series_item <- list(
          data        = yvals,
          type        = "line",
          xAxisIndex  = grid_index,
          yAxisIndex  = grid_index,
          smooth      = TRUE,
          color       = climate_col[[cl]]
        )
        # only one representative series per climate needs a legend entry
        if (m == "BAU" && var == "AverageB.g.m2.") {
          series_item$name <- cl
        }
        series[[length(series) + 1L]] <- series_item
      }
    }
  }
  return(series)
}

#' @export
get_figure <- function(
    combined_data) {
  chart <- echarty$ec.init()
  chart$x$opts <- list(
    legend = list(
      data = c("4.5", "8.5", "current"),
      top = "1%"
    ),
    title = list(
      list(
        left = "8%",
        top = "5%",
        text = "BAU",
        textStyle = list(
          fontSize = 14
        )
      ),
      list(
        left = "22%",
        top = "5%",
        text = "EXT10",
        textStyle = list(
          fontSize = 14
        )
      ),
      list(
        left = "36%",
        top = "5%",
        text = "EXT30",
        textStyle = list(
          fontSize = 14
        )
      ),
      list(
        left = "50%",
        top = "5%",
        text = "GTR30",
        textStyle = list(
          fontSize = 14
        )
      ),
      list(
        left = "64%",
        top = "5%",
        text = "NTLR",
        textStyle = list(
          fontSize = 14
        )
      ),
      list(
        left = "78%",
        top = "5%",
        text = "NTSR",
        textStyle = list(
          fontSize = 14
        )
      ),
      list(
        left = "92%",
        top = "5%",
        text = "SA",
        textStyle = list(
          fontSize = 14
        )
      )
    )
  )

  # append variable labels on the left side
  # chart$x$opts$title <- c(chart$x$opts$title, make_row_titles())

  chart$x$opts$xAxis <- make_x_axes(combined_data)
  chart$x$opts$yAxis <- make_y_axes()
  chart$x$opts$grid <- make_grids()
  chart$x$opts$series <- make_series(combined_data)

  return(chart)
}

make_mc_grids <- function() {
  lapply(0:3, \(i) {
    list(
      left  = sprintf("%d%%", 4 + 24 * i), # 4 %, 28 %, 52 %, 76 %
      top   = "10%",
      width = "20%"
    )
  })
}

make_mc_x_axes <- function(time_vec, i18n) {
  lapply(0:3, \(i) {
    list(
      name          = i18n$t("simulation year"),
      nameLocation  = "middle",
      nameGap       = 30,
      nameTextStyle = list(fontSize = 13, align = "center", color = "black"),
      type          = "category",
      data          = as.character(time_vec),
      gridIndex     = i
    )
  })
}

make_mc_y_axes <- function() {
  y_names <- c("year", "g/m2", "kgDW/m2", "g/m2")
  lapply(0:3, \(i) {
    list(
      name          = y_names[i + 1],
      nameLocation  = "middle",
      nameGap       = 50,
      nameTextStyle = list(fontSize = 13, align = "center", color = "black"),
      type          = "value",
      gridIndex     = i
    )
  })
}

#' @export
get_multichart <- function(experiment_data_file, i18n) {
  # locate & load cohort output
  cohorts_path <- file.path(experiment_data_file, "output", "TotalCohorts.txt")

  # read parameters via helper
  params <- read_landis_params(experiment_data_file)
  start_year <- params$start_year

  data <- utils::read.csv(cohorts_path)
  data$Time <- data$Time + start_year

  # build chart
  chart <- echarty$ec.init()
  chart$x$opts <- list(
    title = list(
      list(left = "8%", top = "1%", text = i18n$t("Average age over time"), textStyle = list(fontSize = 14)),
      list(left = "30%", top = "1%", text = i18n$t("Average above-ground biomass over time"), textStyle = list(fontSize = 14)),
      list(left = "56%", top = "1%", text = i18n$t("Woody debris over time"), textStyle = list(fontSize = 14)),
      list(left = "78%", top = "1%", text = i18n$t("Average below-ground biomass over time"), textStyle = list(fontSize = 14))
    ),
    tooltip = list(trigger = "axis")
  )

  # dynamic axes / grids
  chart$x$opts$grid <- make_mc_grids()
  chart$x$opts$xAxis <- make_mc_x_axes(data$Time, i18n)
  chart$x$opts$yAxis <- make_mc_y_axes()

  # series (unchanged)
  chart$x$opts$series <- list(
    list(
      name = i18n$t("Average age over time"),
      data = data$AverageAge, type = "line", smooth = TRUE, xAxisIndex = 0, yAxisIndex = 0
    ),
    list(
      name = i18n$t("Average above-ground biomass over time"),
      data = data$AverageB.g.m2., type = "line", smooth = TRUE, xAxisIndex = 1, yAxisIndex = 1
    ),
    list(
      name = i18n$t("Woody debris over time"),
      data = data$WoodyDebris.kgDW.m2., type = "line", smooth = TRUE, xAxisIndex = 2, yAxisIndex = 2
    ),
    list(
      name = i18n$t("Average below-ground biomass over time"),
      data = data$AverageBelowGround.g.m2., type = "line", smooth = TRUE, xAxisIndex = 3, yAxisIndex = 3
    )
  )

  chart
}


#' Title
#'
#' @return list, list containing used constant values
init_input_parameters_beehave <- function(){
  df_input_parameters <- data.frame(
    Parameters = c("ABANDON_POLLEN_PATCH_PROB_PER_S", "ENERGY_HONEY_per_g", "MAX_HONEY_STORE_kg", "MAX_BROODCELLS", "MAX_EGG_LAYING", "QueenAgeing", "MAX_km_PER_DAY", "AllowReinfestation", "POLLEN_STORE_INIT", "FORAGER_NURSING_CONTRIBUTION", "SQUADRON_SIZE", "CRITICAL_COLONY_SIZE_WINTER", "MORTALITY_EGGS", "HATCHING_AGE", "MORTALITY_DRONE_EGGS", "DRONE_HATCHING_AGE", "EMERGING_AGE", "MAX_BROOD_NURSE_RATIO", "DRONE_EGGS_PROPORTION", "SEASON_START", "SEASON_STOP", "DRONE_PUPATION_AGE", "DRONE_EMERGING_AGE", "PRE_SWARMING_PERIOD", "N_INITIAL_MITES_HEALTHY", "N_INITIAL_MITES_INFECTED", "POST_SWARMING_PERIOD", "AFF_BASE", "CROPVOLUME", "DRONE_EGGLAYING_START", "DRONE_EGGLAYING_STOP", "MORTALITY_LARVAE", "PUPATION_AGE", "MORTALITY_DRONE_LARVAE", "MORTALITY_PUPAE", "MORTALITY_DRONE_PUPAE", "MIN_AFF", "MAX_AFF", "MORTALITY_INHIVE", "MORTALITY_INHIVE_INFECTED_AS_PUPA", "MORTALITY_DRONES", "DRONE_LIFESPAN", "MAX_TOTAL_KM", "STEPWIDTH", "STEPWIDTHdrones", "FLIGHT_VELOCITY", "MAX_PROPORTION_POLLEN_FORAGERS", "LIFESPAN", "MIN_IDEAL_POLLEN_STORE", "PROTEIN_STORE_NURSES_d", "TIME_NECTAR_GATHERING", "TIME_POLLEN_GATHERING", "TIME_UNLOADING", "TIME_UNLOADING_POLLEN", "N_INITIAL_BEES", "WEIGHT_WORKER_g", "FIND_DANCED_PATCH_PROB", "FLIGHTCOSTS_PER_m", "FORAGING_STOP_PROB", "MAX_DANCE_CIRCUITS", "POLLEN_DANCE_FOLLOWERS", "POLLENLOAD", "MORTALITY_FOR_PER_SEC", "ENERGY_SUCROSE", "N_GENERIC_PLOTS", "MITE_FALL_DRONECELL", "MITE_FALL_WORKERCELL", "MITE_MORTALITY_BROODPERIOD", "MITE_MORTALITY_WINTER", "DISTANCE_G", "DISTANCE_R", "SHIFT_R", "SHIFT_G", "QUANTITY_R_l", "QUANTITY_G_l", "POLLEN_R_kg", "POLLEN_G_kg", "CONC_G", "CONC_R", "DETECT_PROB_G", "DETECT_PROB_R", "DANCE_SLOPE", "DANCE_INTERCEPT"),
    Value = c(2e-05, 12.78, 50, 2000099, 1600, 0, 7299, 0, 100, 0.2, 100, 4000, 0.03, 3, 0.064, 3, 21, 3, 0.04, 1, 365, 10, 24, 3, 0, 0, 0, 21, 50, 115, 240, 0.01, 9, 0.044, 0.001, 0.005, 7, 50, 0.004, 0.012, 0.05, 37, 800, 50, 5, 6.5, 0.8, 290, 250, 7, 1200, 600, 116, 210, 3000, 0.1, 0.5, 6e-06, 0.3, 117, 2, 0.015, 1e-05, 0.00582, 8, 0.2, 0.3, 0.006, 0.002, 500, 1500, 30, -40, 20, 20, 1, 1, 1.5, 1.5, 0.2, 0.2, 1.16, 0),
    `Default Value` = c(2e-05, 12.78, 50, 2000099, 1600, 0, 7299, 0, 100, 0.2, 100, 4000, 0.03, 3, 0.064, 3, 21, 3, 0.04, 1, 365, 10, 24, 3, 0, 0, 0, 21, 50, 115, 240, 0.01, 9, 0.044, 0.001, 0.005, 7, 50, 0.004, 0.012, 0.05, 37, 800, 50, 5, 6.5, 0.8, 290, 250, 7, 1200, 600, 116, 210, 3000, 0.1, 0.5, 6e-06, 0.3, 117, 2, 0.015, 1e-05, 0.00582, 8, 0.2, 0.3, 0.006, 0.002, 500, 1500, 30, -40, 20, 20, 1, 1, 1.5, 1.5, 0.2, 0.2, 1.16, 0)
  )

  return(df_input_parameters)
}



beekeeper_output_plot <- function(input_filepath,
                                  weather_filepath,
                                  color_pallette,
                                  nstep = 730) {
  
  input <- readr::read_csv(input_filepath,
                           col_types = readr::cols())
  
  weather_data <- readr::read_file(weather_filepath) |>
    stringr::str_split(" ",
                       simplify = TRUE)
  weather_data <- weather_data[2:length(weather_data)]
  weather_data <- weather_data[1:(length(weather_data) - 1)]
  
  input <- input |>
    dplyr::filter(`[run number]` == 1,
                  siminputrow == 1,
                  `[step]` %in% 1:nstep) |>
    dplyr::rename(Step = `[step]`)
  input$weather <- rep(weather_data, 2)
  
  pallette <- color_pallette(3)
  
  echarty::ec.init(
    preset = FALSE,
    xAxis = list(type = "value",
                 name = "Step"),
    yAxis = list(
      list(
        type = "value",
        min = 0,
        max = max(input$`(honeyEnergyStore / ( ENERGY_HONEY_per_g * 1000 ))`) + 5,
        name = "Honey (kg)"
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
        name = "Bees count"
      )
    ),
    series = list(
      list(
        type = "bar",
        data = echarty::ec.data(input |> dplyr::select(Step,
                                                       weather)),
        yAxisIndex = 2,
        color = pallette[0], # "green",
        itemStyle = list(opacity = 0.3),
        barWidth = "100%",
        name = "Collection hours"
      ),
      list(
        type = "line",
        showSymbol = FALSE,
        name = "Honey (kg)",
        lineStyle = list(width = 3),
        color = pallette[1], # "gold",
        data = echarty::ec.data(
          input |> dplyr::select(Step,
                                 `Honey (kg)` = `(honeyEnergyStore / ( ENERGY_HONEY_per_g * 1000 ))`)
        )
      ),
      list(
        type = "line",
        showSymbol = FALSE,
        data = echarty::ec.data(input |> dplyr::select(Step,
                                                       `TotalIHbees + TotalForagers`)),
        yAxisIndex = 3,
        lineStyle = list(width = 3),
        color = pallette[2], # "royalblue",
        name = "Bees Count"
      )
    ),
    tooltip = list(show = TRUE,
                   trigger = "axis")
  )
  
}

box::use(
  shiny[NS, tagList, moduleServer, column, fixedRow, reactiveVal, reactive, observeEvent, tags, req],
  bslib[layout_column_wrap],
  htmltools[css],
)

box::use(
  app /
    view /
    grassland /
    grassland_dynamics /
    grassland_dynamics_inputmap[
      grassland_dynamics_inputmap_ui,
      grassland_dynamics_inputmap_server
    ],
  app /
    view /
    grassland /
    grassland_dynamics /
    grassland_dynamics_location[
      grassland_dynamics_location_ui,
      grassland_dynamics_location_server
    ],
  app /
    view /
    grassland /
    grassland_dynamics /
    grassland_dynamics_weather_grass_chart[
      grassland_dynamics_double_chart_ui,
      grassland_dynamics_double_chart_server
    ],
  app /
    view /
    grassland /
    grassland_dynamics /
    grassland_dynamics_weather_grass_chart_controls[
      grassland_dynamics_double_chart_controls_ui,
      grassland_dynamics_double_chart_controls_server
    ],
  app /
    view /
    grassland /
    grassland_dynamics /
    grassland_dynamics_soil_datatable[
      grassland_dynamics_soil_datatable_ui,
      grassland_dynamics_soil_datatable_server
    ],
  app /
    view /
    grassland /
    grassland_dynamics /
    grassland_dynamics_manage_datatable[
      grassland_dynamics_manage_datatable_ui,
      grassland_dynamics_manage_datatable_server
    ],
  app /
    logic /
    grassland /
    grassland_load_simulation_data[load_grassland_simulation_data],
  app /
    view /
    grassland /
    grassland_dynamics /
    grassland_dynamics_three_soil_types[
      grassland_dynamics_three_soil_types_ui,
      grassland_dynamics_three_soil_types_server
    ],
)

#' @export
grassland_dynamics_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = NULL,
      fill = FALSE,
      style = css(grid_template_columns = "3fr 1fr"),
      grassland_dynamics_inputmap_ui(ns("inputmap"), i18n),
      grassland_dynamics_location_ui(ns("location"), i18n),
    ),
    layout_column_wrap(
      width = NULL,
      fill = FALSE,
      style = css(grid_template_columns = "3fr 1fr"),
      grassland_dynamics_double_chart_ui(ns("double_chart"), i18n),
      grassland_dynamics_double_chart_controls_ui(ns("controls"), i18n)
    ),
    grassland_dynamics_three_soil_types_ui(ns("three_soil_types"), i18n),
    grassland_dynamics_manage_datatable_ui(ns("mngmnt_data_table"), i18n),
    grassland_dynamics_soil_datatable_ui(ns("soil_data_table"), i18n)
  )
}

#' @export
grassland_dynamics_server <- function(id, tab_grassland_selected, session_dir, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    plot_type <- reactiveVal("bar")

    # LOCATION settings ----
    # Returns reactive list with: lat, lon, available_runs
    location_data <- grassland_dynamics_location_server("location", i18n, session_dir)

    # Create reactive to extract coordinates for the map
    coordinates <- reactive({
      list(
        lat = location_data$lat(),
        lng = location_data$lon() # Note: map update function expects 'lng' not 'lon'
      )
    })

    # Shared reactive for currently loaded simulation data ----
    current_simulation_data <- reactiveVal(NULL)

    # MAP itself ----
    grassland_dynamics_inputmap_server("inputmap", coordinates, tab_grassland_selected, i18n)

    # Output plot ----
    # Pass available_runs and current_simulation_data to chart module
    grassland_dynamics_double_chart_server(
      "double_chart",
      plot_type,
      location_data$available_runs,
      current_simulation_data,
      tab_grassland_selected,
      i18n
    )

    grassland_dynamics_double_chart_controls_server("controls", plot_type, i18n)

    # Management Actions Table - server module call ----
    # Extract management data from current simulation
    mng_data_table <- reactive({
      sim_data <- current_simulation_data()
      if (is.null(sim_data)) {
        return(NULL)
      }
      sim_data$management_data
    })

    grassland_dynamics_manage_datatable_server(
      "mngmnt_data_table",
      mng_data_table,
      tab_grassland_selected,
      i18n
    )

    # shares of 3 soil "types" (clay, sand and silt) above soil data table
    # to make it easier for users only 2 digits are retained in the UI
    soil_type_shares <- reactive({
      sim_data <- current_simulation_data()
      if (is.null(sim_data)) {
        return(NULL)
      }
      sim_data$soil_shares
    })

    grassland_dynamics_three_soil_types_server(
      "three_soil_types",
      soil_type_shares,
      tab_grassland_selected,
      i18n
    )

    # Soil data table
    soil_data_table <- reactive({
      sim_data <- current_simulation_data()
      if (is.null(sim_data)) {
        return(NULL)
      }
      sim_data$soil_data
    })

    grassland_dynamics_soil_datatable_server(
      "soil_data_table",
      soil_data_table,
      tab_grassland_selected,
      i18n
    )
  })
}

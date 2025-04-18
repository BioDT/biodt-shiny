box::use(
  shiny[NS, tagList, moduleServer, column, fixedRow, reactiveVal, observeEvent, tags],
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
    logic /
    grassland /
    grassland_soil_datatable_modular[
      read_project_config,
      get_soil_file_name,
      get_lat_lon_name,
      get_soil_file_path,
      read_data_table,
      read_main_three_values
    ],
  app /
    view /
    grassland /
    grassland_dynamics /
    grassland_dynamics_soil_main_values[
      grassland_dynamics_soil_main_values_ui,
      grassland_dynamics_soil_main_values_server
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

    grassland_dynamics_soil_main_values_ui(ns("main_soil_values"), i18n),
    grassland_dynamics_soil_datatable_ui(ns("soil_data_table"), i18n)
  )
}

#' @export
grassland_dynamics_server <- function(id, tab_grassland_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    plot_type <- reactiveVal("bar")

    # LOCATION settings ----
    coordinates <- grassland_dynamics_location_server("location")

    # Soil data table
    soil_config <- read_project_config("project1")
    soil_filename <- get_soil_file_name(soil_config)
    soil_lat_lon_path <- get_lat_lon_name(soil_filename)
    soil_file_path <- get_soil_file_path(soil_lat_lon_path)

    soil_data_table <- read_data_table(soil_file_path)

    # Soil - main three values from above table
    main_soil_values <- read_main_three_values(soil_file_path)

    # MAP itself ----
    grassland_dynamics_inputmap_server("inputmap", coordinates, tab_grassland_selected)

    # Output plot ----
    grassland_dynamics_double_chart_server(
      "double_chart",
      plot_type,
      tab_grassland_selected
    )

    grassland_dynamics_double_chart_controls_server("controls", plot_type)

    # Soil data table
    grassland_dynamics_soil_datatable_server(
      "soil_data_table",
      soil_data_table,
      tab_grassland_selected
    )

    # Soil main 3 values from above soil data table
    grassland_dynamics_soil_main_values_server(
      "main_soil_values",
      main_soil_values,
      tab_grassland_selected
    )
  })
}

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
    grassland_soil_management_data_load[
      read_project_config,
      get_soil_file_name,
      get_management_file_name,
      get_lat_lon_name,
      get_file_path,
      read_soil_data_table,
      read_soil_shares,
      read_management_data_table
    ],
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

    # the additional data are displayed as DATA TABLES below the main chart ----
    project_conf <- read_project_config(project_name = "project1")

    # MANAGEMENT data table ----
    mng_filename <- get_management_file_name(project_conf)
    mng_lat_lon_path <- get_lat_lon_name(mng_filename)
    mng_file_path <- get_file_path(type_of_input_file = "management", mng_lat_lon_path)
    mng_data_table <- read_management_data_table(mng_file_path)
    print("mng_data_table:::")
    print(n = 23, mng_data_table) # TODO - remove later

    # SOIL data table (optional - controled by checkbox) ----
    soil_filename <- get_soil_file_name(project_conf)
    soil_lat_lon_path <- get_lat_lon_name(soil_filename)
    soil_file_path <- get_file_path("soil", soil_lat_lon_path)
    soil_data_table <- read_soil_data_table(soil_file_path)

    soil_type_shares <- read_soil_shares(soil_file_path)

    # MAP itself ----
    grassland_dynamics_inputmap_server("inputmap", coordinates, tab_grassland_selected)

    # Output plot ----
    grassland_dynamics_double_chart_server(
      "double_chart",
      plot_type,
      tab_grassland_selected
    )

    grassland_dynamics_double_chart_controls_server("controls", plot_type)

    # shares of 3 soil "types" (clay, sand and silt) above soil data table
    # to make it easier for users only 2 digits are retained in the UI
    grassland_dynamics_three_soil_types_server(
      "three_soil_types",
      soil_type_shares,
      tab_grassland_selected
    )

    # Soil data table
    grassland_dynamics_soil_datatable_server(
      "soil_data_table",
      soil_data_table,
      tab_grassland_selected
    )
  })
}

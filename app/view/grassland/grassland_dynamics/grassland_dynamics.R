box::use(
  shiny[NS, tagList, moduleServer],
  bslib[layout_column_wrap],
  htmltools[css],
)

box::use(
  app/view/grassland/grassland_dynamics/grassland_dynamics_inputmap[
    grassland_dynamics_inputmap_ui,
    grassland_dynamics_inputmap_server
  ],
  app/view/grassland/grassland_dynamics/grassland_dynamics_location[
    grassland_dynamics_location_ui,
    grassland_dynamics_location_server
  ],
  app/view/grassland/grassland_dynamics/grassland_dynamics_outputplot[
    grassland_dynamics_outputplot_ui,
    grassland_dynamics_outputplot_server
  ],
  app/view/grassland/grassland_dynamics/grassland_dynamics_datachart[
    grassland_dynamics_datachart_ui,
    grassland_dynamics_datachart_server
  ],
  app/view/grassland/grassland_dynamics/grassland_dynamics_soil_datatable[
    grassland_dynamics_soil_datatable_ui,
    grassland_dynamics_soil_datatable_server
  ],
  app/logic/grassland/grassland_soil_datatable_modular[
    read_project_config,
    get_soil_file_name,
    get_lat_lon_name,
    get_soil_file_path,
    read_data_table,
    read_main_three_values
  ],
  app/view/grassland/grassland_dynamics/grassland_dynamics_soil_main_values[
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
    grassland_dynamics_outputplot_ui(ns("outputplot"), i18n),
    grassland_dynamics_datachart_ui(ns("datachart"), i18n), # UI wrapper for the chart of use case's variable(s) (currently "PFTs")
    grassland_dynamics_soil_main_values_ui(ns("main_soil_values"), i18n),
    grassland_dynamics_soil_datatable_ui(ns("soil_data_table"), i18n)
  )
}

#' @export
grassland_dynamics_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # LOCATION settings ----
    coordinates <- grassland_dynamics_location_server("location")

    # Soil data table
    config <- read_project_config("project1")
    filename <- get_soil_file_name(config)
    lat_lon_path <- get_lat_lon_name(filename)
    soil_file_path <- get_soil_file_path(lat_lon_path)
    soil_data_table <- read_data_table(soil_file_path)

    # Soil - main three values from above table
    main_soil_values <- read_main_three_values(soil_file_path)

    # MAP itself ----
    grassland_dynamics_inputmap_server("inputmap", coordinates)

    # Output PLOT ----
    grassland_dynamics_outputplot_server("outputplot")

    # Module with logic for a displaying of Grassland's data
    grassland_dynamics_datachart_server("datachart")

    # Soil data table
    grassland_dynamics_soil_datatable_server("soil_data_table", soil_data_table)

    # Soil main 3 values from above soil data table
    grassland_dynamics_soil_main_values_server("main_soil_values", main_soil_values)
  })
}

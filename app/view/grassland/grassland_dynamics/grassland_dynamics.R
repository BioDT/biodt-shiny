box::use(
  shiny[NS, tagList, moduleServer, column, fixedRow, reactiveVal],
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
  app/view/grassland/grassland_dynamics/grassland_dynamics_datachart_controls[
    grassland_dynamics_datachart_controls_ui,
    grassland_dynamics_datachart_controls_server
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
    layout_column_wrap(
      width = NULL,
      fill = FALSE,
      style = css(grid_template_columns = "3fr 1fr"),
      grassland_dynamics_datachart_ui(ns("datachart"), i18n), # UI wrapper for the chart of use case's variable(s) (currently "PFTs")
      grassland_dynamics_datachart_controls_ui(ns("controls"), i18n)
    )
  )
}

#' @export
grassland_dynamics_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    plot_type <- reactiveVal("bar")
    mean_switch <- reactiveVal(FALSE)

    # LOCATION settings ----
    coordinates <- grassland_dynamics_location_server("location")

    # MAP itself ----
    grassland_dynamics_inputmap_server("inputmap", coordinates)

    # Output PLOT ----
    grassland_dynamics_outputplot_server("outputplot")

    # Module with logic for a displaying of Grassland's data
    grassland_dynamics_datachart_server("datachart", plot_type, mean_switch)

    grassland_dynamics_datachart_controls_server("controls", plot_type, mean_switch)
  })
}

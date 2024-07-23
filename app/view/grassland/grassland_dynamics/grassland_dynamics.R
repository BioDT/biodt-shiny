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
    grassland_dynamics_outputplot_ui(ns("outputplot"), i18n)
  )
}

#' @export
grassland_dynamics_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # LOCATION settings ----
    coordinates <- grassland_dynamics_location_server("location")

    # MAP itself ----
    grassland_dynamics_inputmap_server("inputmap", coordinates)

    # Output PLOT ----
    grassland_dynamics_outputplot_server("outputplot")
  })
}

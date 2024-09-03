box::use(
  shiny[NS, tagList, moduleServer, tags],
  bslib[layout_column_wrap],
  htmltools[css],
)

box::use(
  app/view/disease_outbreaks/disease_app/disease_map[disease_map_ui, disease_map_server],
)


#' @export
disease_app_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = NULL,
      fill = FALSE,
      style = css(grid_template_columns = "3fr 1fr"),
      disease_map_ui(
        ns("map"), i18n
      ),
      tags$h3(i18n$translate("Location")),
    ),
    tags$h3(i18n$translate("Output Plot"))
  )
}

#' @export
disease_app_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # # LOCATION settings ----
    # coordinates <- grassland_dynamics_location_server("location")

    # # MAP itself ----
    # grassland_dynamics_inputmap_server("inputmap", coordinates)

    # # Output PLOT ----
    # grassland_dynamics_outputplot_server("outputplot")
  })
}

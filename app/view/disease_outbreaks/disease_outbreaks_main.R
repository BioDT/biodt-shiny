box::use(
  shiny[moduleServer, icon, NS],
  bslib[navset_tab, nav_panel],
)

box::use(
  app/view/disease_outbreaks/info/disease_info[disease_info_ui, disease_info_server]
)

#' @export
disease_outbreaks_main_ui <- function(id) {
  ns <- NS(id)
  navset_tab(
    # Info Page ---
    nav_panel(
      title = "Info",
      value = "Info",
      icon = icon("circle-info"),
      disease_info_ui(
        ns("disease_info_ui")
      )
    ),
    # Grassland Case ----
    # nav_panel(
    #   title = "Disease Outbreaks",
    #   icon = icon("disease"),
    #   grassland_dynamics_ui(
    #     ns("disease_outbreaks_app")
    #   )
    # )
  )
}

#' @export
disease_outbreaks_main_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # disease_info_server("grassland_app")
  })
}

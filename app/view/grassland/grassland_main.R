box::use(
  shiny[moduleServer, icon, NS],
  bslib[navset_tab, nav_panel],
)

box::use(
  app / view / grassland / grassland_dynamics / grassland_dynamics[grassland_dynamics_ui,
                                                                   grassland_dynamics_server],
  app / view / grassland / info / grassland_info[grassland_info_ui],
)

#' @export
grassland_main_ui <- function(id, theme) {
  ns <- NS(id)

  navset_tab(
    # Info Page ---
    nav_panel(
      title = "Info",
      value = "Info",
      icon = icon("circle-info"),
      grassland_info_ui(
        ns("grassland_info")
      )
    ),
    # Grassland Case ----
    nav_panel(
      title = "Grassland Dynamics",
      icon = icon("leaf"),
      grassland_dynamics_ui(
        ns("grassland_app")
      )
    )
  )
}

#' @export
grassland_main_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    grassland_dynamics_server("grassland_app")
  })
}

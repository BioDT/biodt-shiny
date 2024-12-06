box::use(
  shiny[moduleServer, icon, NS],
  bslib[navset_tab, nav_panel],
)

box::use(
  app/view/grassland/grassland_dynamics/grassland_dynamics[
    grassland_dynamics_ui,
    grassland_dynamics_server
  ],
  app/view/grassland/info/grassland_info[grassland_info_ui],
)

#' @export
grassland_main_ui <- function(id, i18n) {
  ns <- NS(id)
  navset_tab(
    # Info Page ---
    nav_panel(
      title = "Info",
      value = "Info",
      icon = icon("circle-info"),
      grassland_info_ui(
        ns("grassland_info"),
        i18n
      )
    ),
    # Grassland Case ----
    nav_panel(
      title = i18n$translate("Grassland Dynamics"),
      value = "Grassland Dynamics",
      icon = icon("leaf"),
      grassland_dynamics_ui(
        ns("grassland_app"),
        i18n
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

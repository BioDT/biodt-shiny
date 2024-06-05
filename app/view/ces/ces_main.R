box::use(
  shiny[moduleServer, icon, NS],
  bslib[navset_tab, nav_panel],
)

box::use(
  app/view/ces/ces_info[ces_info_ui],
)

#' @export
ces_ui <- function(id) {
  ns <- NS(id)
  navset_tab(
    # Info Page ---
    nav_panel(
      title = "Info",
      value = "Info",
      icon = icon("circle-info"),
      ces_info_ui(
        ns("ces_info")
      )
    )
  )
}

#' @export
ces_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ces_dynamics_server("ces_app")
  })
}

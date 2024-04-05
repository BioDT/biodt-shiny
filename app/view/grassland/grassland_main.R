box::use(
  shiny,
  bslib[navset_tab, nav_panel]
)

box::use(
  app/view/grassland/grassland_app[grassland_app_ui, grassland_app_server],
  app/view/grassland/grassland_info[grassland_info_ui, grassland_info_server],
)

#' @export
grassland_ui <- function(id, theme) {
  ns <- shiny::NS(id)

    shiny::tagList(
      navset_tab(
        # Info Page ---
        nav_panel(
          title = "Info",
          value = "Info",
          icon = shiny$icon("circle-info"),
          grassland_info_ui(
            ns("grassland_info")
          )
        ),
        # Grassland Case ----
        nav_panel(
          title = "Grassland Dynamics",
          icon = shiny$icon("leaf"),
          grassland_app_ui(
            ns("grassland_app"),
            theme
          )
        )
      )
    )
}

#' @export
grassland_server <- function(id, r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      grassland_info_server("grassland_info", r)
      grassland_app_server("grassland_app", r)
    }
  )
}

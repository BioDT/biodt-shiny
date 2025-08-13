box::use(
  shiny[moduleServer, icon, NS, reactiveVal, observeEvent],
  bslib[navset_tab, nav_panel],
)

box::use(
  app /
    view /
    grassland /
    grassland_dynamics /
    grassland_dynamics[
      grassland_dynamics_ui,
      grassland_dynamics_server
    ],
  app / view / grassland / info / grassland_info[grassland_info_ui, grassland_info_server],
  app / view / grassland / grassland_contributors[grassland_contributors_ui, grassland_contributors_server],
)

#' @export
grassland_main_ui <- function(id, i18n) {
  ns <- NS(id)
  navset_tab(
    id = (ns("tab")),
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
      value = "grassland_app",
      icon = icon("leaf"),
      grassland_dynamics_ui(
        ns("grassland_app"),
        i18n
      )
    ),
    nav_panel(
      title = i18n$translate("Contributors"),
      value = "Contributors",
      icon = icon("leaf"),
      grassland_contributors_ui(
        ns("grassland_contributors"),
        i18n
      )
    )
  )
}

#' @export
grassland_main_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tab_grassland_selected <- reactiveVal(FALSE)

    observeEvent(
      input$tab,
      {
        if (input$tab == "grassland_app") {
          tab_grassland_selected(TRUE)
        } else {
          tab_grassland_selected(FALSE)
        }
      }
    )

    grassland_dynamics_server("grassland_app", tab_grassland_selected, i18n)
    grassland_info_server("grassland_info", session)
  })
}

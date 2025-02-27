box::use(
  shiny[moduleServer, icon, NS, reactiveVal, observeEvent],
  bslib[navset_tab, nav_panel],
)

box::use(
  app/view/ias/ias_info[ias_info_ui, ias_info_server],
  app/view/ias/ias_app[ias_app_ui, ias_app_server],
  app/view/ias/ias_contributors[ias_contributors_ui,ias_contributors_server],
)

#' @export
ias_ui <- function(id, i18n) {
  ns <- NS(id)
  navset_tab(
    id = ns("tab"),
    # Info Page ---
    nav_panel(
      title = "Info",
      value = "Info",
      icon = icon("circle-info"),
      ias_info_ui(
        ns("ias_info")
      )
    ),
    nav_panel(
      title = "IAS App",
      value = "IAS App",
      icon = icon("tree"),
      ias_app_ui(
        ns("ias_app")
      )
    ),
    nav_panel(
      title = "Contributors",
      value = "Contributors",
      icon = icon("tree"),
      ias_contributors_ui(
        ns("ias_contributors")
      )
    )
  )
}

#' @export
ias_main_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tab <- reactiveVal(FALSE)

    observeEvent(
      input$tab,
      ignoreInit = TRUE, {
      print(input$tab)
      tab(input$tab)
    })

    ias_info_server(
      "ias_info",
      session
    )

    ias_app_server(
      "ias_app",
      tab_selected = tab
    )
  })
}

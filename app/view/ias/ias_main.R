box::use(
  shiny[moduleServer, icon, NS, reactiveVal, observeEvent, observe, req],
  bslib[navset_tab, nav_panel],
)

box::use(
  app / view / ias / ias_info[ias_info_ui, ias_info_server],
  app / view / ias / ias_app[ias_app_ui, ias_app_server],
  app / view / ias / ias_contributors[ias_contributors_ui, ias_contributors_server],
)

#' @export
ias_main_ui <- function(id, i18n) {
  ns <- NS(id)

  # Add explicit selected parameter to set initial tab
  navset_tab(
    id = ns("tab"),
    selected = "Info", # Explicitly set initial selection
    # Info Page ---
    nav_panel(
      title = i18n$translate("Info"),
      value = "Info",
      icon = icon("circle-info"),
      ias_info_ui(
        ns("ias_info"),
        i18n
      )
    ),
    nav_panel(
      title = i18n$translate("IAS App"),
      value = "ias_app",
      icon = icon("tree"),
      ias_app_ui(
        ns("ias_app"),
        i18n
      )
    ),
    nav_panel(
      title = i18n$translate("Contributors"),
      value = "Contributors",
      icon = icon("tree"),
      ias_contributors_ui(
        ns("ias_contributors"),
        i18n
      )
    )
  )
}

#' @export
ias_main_server <- function(id, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tab_ias_selected <- reactiveVal(FALSE)

    # Add JavaScript to manually update Shiny input when tabs change
    session$onFlushed(
      function() {
        session$sendCustomMessage("ias-setup-tab-listener", list(id = ns("tab")))
      },
      once = TRUE
    )

    observe({
      req(input$tab)

      if (input$tab == "ias_app") {
        tab_ias_selected(TRUE)
      } else {
        tab_ias_selected(FALSE)
      }
    })

    ias_info_server(
      "ias_info",
      session,
      i18n
    )

    ias_app_server(
      "ias_app",
      tab_selected = tab_ias_selected,
      i18n
    )
  })
}

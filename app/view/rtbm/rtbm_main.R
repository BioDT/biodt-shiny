box::use(
  shiny[moduleServer, icon, NS, reactiveVal, observeEvent],
  bslib[navset_tab, nav_panel],
)

box::use(
  app / view / rtbm / rtbm_info[rtbm_info_ui, rtbm_info_server],
  app / view / rtbm / rtbm_app[rtbm_app_ui, rtbm_app_server],
  app / view / rtbm / rtbm_contributors[rtbm_contributors_ui, rtbm_contributors_server],
)

#' @export
rtbm_ui <- function(id, i18n) {
  ns <- NS(id)
  navset_tab(
    id = ns("tab"),
    # Info Page ---
    nav_panel(
      title = i18n$t("Info"),
      value = "Info",
      icon = icon("circle-info"),
      rtbm_info_ui(
        ns("rtbm_info"),
        i18n
      )
    ),
    nav_panel(
      title = i18n$t("RTBM App"),
      value = "rtbm_app",
      icon = icon("tree"),
      rtbm_app_ui(
        ns("rtbm_app"),
        i18n
      )
    ),
    nav_panel(
      title = i18n$t("Contributors"),
      value = "Contributors",
      icon = icon("tree"),
      rtbm_contributors_ui(
        ns("rtbm_contributors"),
        i18n
      )
    )
  )
}

#' @export
rtbm_main_server <- function(id, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tab <- reactiveVal(FALSE)

    observeEvent(
      input$tab,
      ignoreInit = TRUE,
      {
        tab(input$tab)
      }
    )

    rtbm_info_server(
      "rtbm_info",
      session
    )

    rtbm_app_server(
      "rtbm_app",
      tab_selected = tab,
      i18n
    )
  })
}

box::use(
  shiny[moduleServer, NS, tagList, tags, actionButton, reactiveVal, observeEvent, icon],
  echarty[ec.init],
  bslib[navset_tab, nav_panel],
)

box::use(
  app / view / forest / forest_info[forest_info_ui, forest_info_server],
  app / view / forest / forest_app[forest_app_ui, forest_app_server],
  app / view / forest / forest_contributors[forest_contributors_ui],
)

#' @export
forest_main_ui <- function(id, i18n) {
  ns <- NS(id)
  navset_tab(
    id = ns("tab"),
    # Info Page ---
    nav_panel(
      title = i18n$translate("Info"),
      value = "Info",
      icon = icon("circle-info"),
      forest_info_ui(
        ns("forest_info"),
        i18n
      )
    ),
    # Forest Case ----
    nav_panel(
      title = i18n$translate("App"),
      value = "App",
      icon = icon("tree"),
      forest_app_ui(
        ns("forest_app"),
        i18n
      )
    ),
    # Forest Contributors ----
    nav_panel(
      title = i18n$translate("Contributors"),
      value = "Contributors",
      icon = icon("sitemap"),
      forest_contributors_ui(
        ns("forest_contributors"),
        i18n
      )
    )
  )
}

#' @export
forest_main_server <- function(id, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    app_selected <- reactiveVal(FALSE)

    observeEvent(
      input$tab,
      {
        print(input$tab)
        if (input$tab == "App") {
          app_selected(TRUE)
        } else {
          app_selected(FALSE)
        }
        print(app_selected())
      }
    )

    forest_info_server(
      "forest_info",
      session
    )

    forest_app_server(
      "forest_app",
      app_selected,
      i18n
    )
  })
}

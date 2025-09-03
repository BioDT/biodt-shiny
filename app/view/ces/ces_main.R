box::use(
  shiny[moduleServer, icon, NS, reactiveVal, observeEvent],
  bslib[navset_tab, nav_panel],
)

box::use(
  app / view / ces / ces_info[ces_info_ui, ces_info_server],
  app / view / ces / ces_rp[ces_rp_ui, ces_rp_server],
  app / view / ces / ces_contributors[ces_contributors_ui, ces_contributors_server],
  app / view / ces / ces_biodiversity[ces_biodiversity_ui, ces_biodiversity_server],
  app / view / ces / ces_rp_biodiversity[ces_rp_biodiversity_ui, ces_rp_biodiversity_server],
)

#' @export
ces_ui <- function(id, i18n) {
  ns <- NS(id)
  navset_tab(
    id = ns("tab"),
    # Info Page ---
    nav_panel(
      title = i18n$t("Info"),
      value = "Info",
      icon = icon("circle-info"),
      ces_info_ui(
        ns("ces_info"),
        i18n
      )
    ),
    nav_panel(
      title = i18n$t("Recreation and Biodiversity"),
      value = "CES",
      icon = icon("tree"),
      ces_rp_biodiversity_ui(
        ns("ces_rp_biodiversity"),
        i18n
      )
    ),
    nav_panel(
      title = i18n$t("Recreation Potential"),
      value = "Recreation potential",
      icon = icon("person-walking"),
      ces_rp_ui(
        ns("ces_rp"),
        i18n
      )
    ),
    nav_panel(
      title = i18n$t("Biodiversity"),
      value = "Biodiversity",
      icon = icon("tree"),
      ces_biodiversity_ui(
        ns("ces_biodiversity"),
        i18n
      )
    ),
    nav_panel(
      title = i18n$t("Contributors"),
      value = "Contributors",
      icon = icon("sitemap"),
      ces_contributors_ui(
        ns("ces_contributors"),
        i18n
      )
    ),
  )
}

#' @export
ces_server <- function(id, i18n, language_change) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to track if the tabs were selected
    ces_selected <- reactiveVal(FALSE)

    observeEvent(
      input$tab,
      ignoreInit = TRUE,
      {
        print(input$tab)
        # Check if the tab matches any of the specified tabs
        if (
          !ces_selected() &&
            (input$tab == "CES")
          # || input$tab == "Recreation potential" ||
          # input$tab == "Biodiversity"
        ) {
          # Set ces_selected to TRUE if it's not already TRUE
          ces_selected(TRUE)
        }
      }
    )

    # Call downstream module servers only the first time
    ces_rp_server("ces_rp")
    ces_biodiversity_server("ces_biodiversity")
    ces_rp_biodiversity_server(
      "ces_rp_biodiversity",
      ces_selected = ces_selected,
      i18n,
      language_change
    )

    ces_info_server("ces_info", session)
  })
}

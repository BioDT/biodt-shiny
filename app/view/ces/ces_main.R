box::use(
  shiny[moduleServer, icon, NS, reactiveVal, observeEvent],
  bslib[navset_tab, nav_panel],
)

box::use(
  app / view / ces / ces_info[ces_info_ui, ces_info_server],
  app / view / ces / ces_contributors[ces_contributors_ui, ces_contributors_server],
  app / view / ces / ces_biodiversity[ces_biodiversity_ui, ces_biodiversity_server],
  app / view / ces / ces_rp_biodiversity[ces_rp_biodiversity_ui, ces_rp_biodiversity_server],
  app / view / ces / ces_persona[ces_persona_ui, ces_persona_server],
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
      title = i18n$t("Personalised Recreation"),
      value = "CES_Persona",
      icon = icon("tree"),
      ces_persona_ui(
        ns("ces_persona"),
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
        ces_selected(input$tab)
      }
    )

    # Call downstream module servers only the first time
    ces_biodiversity_server("ces_biodiversity", ces_selected, i18n)
    ces_rp_biodiversity_server(
      "ces_rp_biodiversity",
      ces_selected = ces_selected,
      i18n,
      language_change
    )

    ces_persona_server(
      "ces_persona",
      ces_selected = ces_selected,
      i18n,
      language_change
    )

    ces_info_server("ces_info", session)
  })
}

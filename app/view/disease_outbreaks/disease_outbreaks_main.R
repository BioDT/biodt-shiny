box::use(
  shiny[moduleServer, icon, NS],
  bslib[navset_tab, nav_panel],
)

box::use(
  app/view/disease_outbreaks/info/disease_info[disease_info_ui],
  app/view/disease_outbreaks/disease_app/disease_app[disease_app_ui, disease_app_server],
  app/view/disease_outbreaks/disease_contributors[disease_contributors_ui],
)

#' @export
disease_outbreaks_main_ui <- function(id, i18n) {
  ns <- NS(id)
  navset_tab(
    # Info Page ---
    nav_panel(
      title = i18n$translate("Info"),
      value = "Info (called from 1st navpanel in disease_outbreaks_main.R)",
      icon = icon("circle-info"),
      disease_info_ui(
        ns("disease_info"), i18n
      )
    ),
    nav_panel(
      title = i18n$translate("Disease Outbreaks"),
      icon = icon("bugs"),
      disease_app_ui(
        ns("disease_app"), i18n
      )
    ),
    nav_panel(
      title = i18n$translate("Contributors"),
      value = "Contributors",
      icon = icon("sitemap"),
      disease_contributors_ui(
        ns("disease_contributors"),
        i18n
      )
    )
  )
}

#' @export
disease_outbreaks_main_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    disease_app_server("disease_app")
  })
}

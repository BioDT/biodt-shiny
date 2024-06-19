box::use(
  shiny[moduleServer, NS, tagList, tags, actionButton, reactiveVal, observeEvent, icon],
  echarty[ec.init],
  bslib[navset_tab, nav_panel],
)

box::use(
  app/view/honeybee/honeybee_info[honeybee_info_ui, honeybee_info_server],
  app/view/honeybee/honeybee_beekeeper[honeybee_beekeeper_ui, honeybee_beekeeper_server],
  app/view/honeybee/beekeeper_contributors[bee_contributors_ui],
)
#' @export
honeybee_ui <- function(id,
                        theme) {
  ns <- NS(id)
  # tagList(
  navset_tab(
    id = ns("tab"),
    # Info Page ---
    nav_panel(
      title = "Info",
      icon = icon("circle-info"),
      honeybee_info_ui(ns("honeybee_info"))
    ),
    # Beekeper Case ----
    nav_panel(
      title = "Beekeeper",
      icon = icon("forumbee"),
      honeybee_beekeeper_ui(
        ns("honeybee_beekeeper"),
        theme
      )
    ),
    # Beekeper Contributors ----
    nav_panel(
      title = "Contributors",
      icon = icon("sitemap"),
      bee_contributors_ui(
        ns("beekeeper_contributors")
      )
    ),
  )
  # )
}

#' @export
honeybee_server <- function(id,
                            session_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    beekeeper_selected <- reactiveVal(FALSE)

    observeEvent(
      input$tab,
      {
        if (input$tab == "Beekeeper") {
          beekeeper_selected(TRUE)
        } else {
          beekeeper_selected(FALSE)
        }
      }
    )

    honeybee_info_server(
      "honeybee_info",
      session
    )

    honeybee_beekeeper_server(
      "honeybee_beekeeper",
      session_dir,
      beekeeper_selected
    )
  })
}

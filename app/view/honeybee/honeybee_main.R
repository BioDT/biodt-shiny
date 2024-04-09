box::use(
  shiny[moduleServer, NS, tagList, tags, actionButton, reactiveVal, observeEvent, icon],
  echarty[ec.init],
  bslib[navset_tab, nav_panel],
)

box::use(
  app/view/honeybee/honeybee_info[honeybee_info_ui],
  app/view/honeybee/honeybee_beekeeper[honeybee_beekeeper_ui, honeybee_beekeeper_server],
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
    )
  )
  # )
}

#' @export
honeybee_server <- function(id,
                            r) {
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

    honeybee_beekeeper_server(
      "honeybee_beekeeper",
      r,
      beekeeper_selected
    )
  })
}

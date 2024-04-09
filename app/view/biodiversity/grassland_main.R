box::use(
  shiny,
  bslib[navset_tab, nav_panel]
)

box::use(
  app/view/biodiversity/grassland/app,
  app/view/biodiversity/grassland/info,
)

#' @export
ui <- function(id, theme) {
  ns <- shiny::NS(id)

  navset_tab(
    # Info Page ---
    nav_panel(
      title = "Info",
      value = "Info",
      icon = shiny$icon("circle-info"),
      info$ui(
        ns("info")
      )
    ),
    # Grassland Case ----
    nav_panel(
      title = "Grassland Dynamics",
      icon = shiny$icon("leaf"),
      app$ui(
        ns("app")
      )
    )
  )
}

#' @export
server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      info$server(ns("info"))
      app$server(ns("app"))
    }
  )
}

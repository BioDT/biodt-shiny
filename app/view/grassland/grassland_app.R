box::use(
  shiny[NS, tagList, moduleServer, tags, reactiveVal, observeEvent],
  bslib[page_fluid, layout_sidebar, navset_tab, layout_column_wrap],
  htmltools[css],
)

box::use(
  app/view/grassland/grassland_inputmap[grassland_inputmap_ui, grassland_inputmap_server],
  app/view/grassland/grassland_location[grassland_location_ui, grassland_location_server],
  app/view/grassland/grassland_outputplot[grassland_outputplot_ui, grassland_outputplot_server],
  app/view/grassland/grassland_control[grassland_control_ui, grassland_control_server],
  app/logic/grassland/leaflet_inputmap[leaflet_inputmap]
)

#' @export
grassland_app_ui <- function(id, theme) {
  ns <- shiny::NS(id)

  tagList(
    grassland_control_ui(ns("grassland_control")),
    layout_column_wrap(
      width = NULL,
      fill = FALSE,
      style = htmltools::css(grid_template_columns = "3fr 1fr"),
      grassland_inputmap_ui(ns("grassland_inputmap")),
      grassland_location_ui(ns("grassland_location")),
    ),
    grassland_outputplot_ui(ns("grassland_outputplot"))
  )
}

#' @export
grassland_app_server <- function(id, r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      map <- reactiveVal()

      leaflet_inputmap(location_server()) |>
        map()
      
      inputmap_server <- grassland_inputmap_server(
        "grassland_inputmap",
        leaflet_map = map
      )

      observeEvent(
        inputmap_server(),
        print("Grassland `inputmap_server`::: ", inputmap_server())
      )
      
      
      location_server <- grassland_location_server("grassland_location")

      observeEvent(
        location_server(),
        print(location_server())
      )

      grassland_outputplot_server("grassland_outputplot", r)
    }
  )
}
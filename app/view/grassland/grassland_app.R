box::use(
  shiny[NS, tagList, moduleServer],
  bslib[page_fluid, layout_sidebar, navset_tab],
)

box::use(
  app/view/grassland/grassland_inputmap[mod_grassland_inputmap_ui, mod_grassland_inputmap_server],
  app/view/grassland/grassland_location[mod_grassland_location_ui, mod_grassland_location_server],
  app/view/grassland/grassland_outputplot[mod_grassland_outputplot_ui, mod_grassland_outputplot_server],
)

#' @export
grassland_app_ui <- function(id, theme) {
  ns <- shiny::NS(id)


  bslib::page_fluid(
    theme = theme,
    class = "p-0",
      navset_tab(
        bslib::layout_sidebar(
          border = FALSE,
          mod_grassland_location_ui(ns("grassland_location"))
        ),
        mod_grassland_inputmap_ui(ns("grassland_inputmap")),
        mod_grassland_outputplot_ui(ns("grassland_outputplot"))
      )
  )
}

#' @export
grassland_app_server <- function(id, r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      mod_grassland_location_server(ns("grassland_location"), r)
      mod_grassland_inputmap_server(ns("grassland_inputmap"), r)
      mod_grassland_outputplot_server(ns("grassland_outputplot"), r)
    }
  )
}
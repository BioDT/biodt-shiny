box::use(shiny[NS, tagList],
         bslib[page_fluid])

box::use(
  app/main[biodt_theme],
  app/view/grassland/grassland_location[mod_grassland_location_ui, mod_grassland_location_server],
  app/view/grassland/grassland_inputmap[mod_grassland_inputmap_ui, mod_grassland_inputmap_server],
  app/view/grassland/grassland_outputplot[mod_grassland_outputplot_ui, mod_grassland_outputplot_server],
)

#' @export
grassland_main_ui <- function(id, theme) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::page_fluid(
      theme = theme,
      class = "p-0",
      mod_grassland_location_ui(ns("grassland_location")),
      mod_grassland_inputmap_ui(ns("grassland_inputmap")),
      mod_grassland_outputplot_ui(ns("grassland_outputplot"))
    )
  )
}

#' @export
grassland_main_server <- function(id, r) {
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

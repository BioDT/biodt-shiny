box::use(shiny[NS, tagList],
         bslib[page_fluid])

box::use(
  app/main[biodt_theme],
  app/view/grassland/grassland_location[mod_grassland_location_ui, mod_grassland_location_server],
  app/view/grassland/grassland_inputmap[mod_grassland_inputmap_ui, mod_grassland_inputmap_server],
  app/view/grassland/grassland_outputplot[mod_grassland_outputplot_ui, mod_grassland_outputplot_server],
)

#' @export
grassland_main_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_fluid(
      theme = biodt_theme,
      class = "p-0",
      mod_grassland_location_ui,
      mod_grassland_inputmap_ui,
      mod_grassland_outputplot_ui
    )
  )
}

#' @export
grassland_main_server <- function(id, r) {
  
  mod_grassland_location_server(id, r)
  
  mod_grassland_inputmap_server(id, r)
  
  mod_grassland_outputplot_server(id, r)
}

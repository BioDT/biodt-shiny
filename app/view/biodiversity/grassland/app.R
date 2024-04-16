box::use(
  shiny[NS, tagList, moduleServer],
  bslib[layout_column_wrap],
  htmltools[css],
)

box::use(
  app/view/biodiversity/grassland/inputmap,
  app/view/biodiversity/grassland/location,
  # TODO
  # app/view/biodiversity/grassland/grassland_outputplot[grassland_outputplot_ui, grassland_outputplot_server],
  app/view/biodiversity/grassland/control,
  app/logic/grassland/update_inputmap,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    control$ui(ns("control")),    
    layout_column_wrap(
      width = NULL,
      fill = FALSE,
      style = css(grid_template_columns = "3fr 1fr"),
      inputmap$ui(ns("inputmap")),
      location$ui(ns("location")),
    ),
    # grassland_outputplot_ui(ns("grassland_outputplot"))
  )
}

#' @export
server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # LOCATION settings ----
      location$server("location") 
      
      # MAP itself ----
      inputmap$server("inputmap")
    }
  )
}
box::use(
  shiny[NS, tagList, moduleServer, observeEvent, actionButton],
  bslib[layout_column_wrap],
  htmltools[css],
)

box::use(
  app/view/biodiversity/grassland/inputmap,
  app/view/biodiversity/grassland/location,
  # TODO
  # app/view/biodiversity/grassland/grassland_outputplot[grassland_outputplot_ui, grassland_outputplot_server],
  app/view/biodiversity/grassland/control,
  app/logic/grassland/update_inputmap[update_inputmap],
  app/view/biodiversity/grassland/test,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    control$ui(ns("control")),    
    actionButton(
      ns("update_map"),
      label = "Update map",
      width = "100%",
      class = "btn-secondary px-2 mx-2",
    ),
    layout_column_wrap(
      width = NULL,
      fill = FALSE,
      style = css(grid_template_columns = "3fr 1fr"),
      inputmap$ui(
        ns("map")
      ),
      location$ui(ns("location")),
    ),
    test$ui(ns("test")),
    # grassland_outputplot_ui(ns("grassland_outputplot"))
  )
}

#' @export
server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # setting LOCATION ----
      location$server("location")      
      
      map_defaults <- list(
        lng = 11.8787,
        lat = 51.3919,
        zoom = 9
      )

      inputmap$server("map")

      observeEvent(input$update_map, {
        update_inputmap(ns("map"), map_defaults)  
      })

      # grassland_outputplot_server("grassland_outputplot", r)

      # JUST TESTING STUFF... OF MINE lerning - TODO remove later
      test$server("test")
    }
  )
}
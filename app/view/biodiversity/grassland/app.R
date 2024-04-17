box::use(
  shiny[NS, tagList, moduleServer],
  bslib[layout_column_wrap],
  htmltools[css],
)

box::use(
  app/view/biodiversity/grassland/inputmap,
  app/view/biodiversity/grassland/location,
  app/view/biodiversity/grassland/outputplot,
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_column_wrap(
      width = NULL,
      fill = FALSE,
      style = css(grid_template_columns = "3fr 1fr"),
      inputmap$ui(
        ns("inputmap")
      ),
      location$ui(
        ns("location")
      ),
    ),
    outputplot$ui(
      ns("outputplot")
    )
  )
}

#' @export
server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
      ns <- session$ns

      # LOCATION settings ----
      coordinates <- location$server("location") 
      
      # MAP itself ----
      inputmap$server("inputmap", coordinates)

      # Output PLOT ----
      outputplot$server("outputplot")
    }
  )
}
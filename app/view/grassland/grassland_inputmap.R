box::use(
  shiny[NS, moduleServer, tags, observeEvent],
  bslib[card, card_header, card_body],
  leaflet[leafletOutput, renderLeaflet],
)

#' @export
grassland_inputmap_ui <- function(id) {
  ns <- NS(id)
  card(
    class = "ms-md-3 card-shadow",
    id = ns("input_map"),
    full_screen = TRUE,
    card_header(
      tags$h5("Input Map")
    ),
    card_body(
      id = ns("input_map_cardbody"),
      leafletOutput(
        ns("input_map")
      )
    )
  )
}

#' @export
grassland_inputmap_server <- function(id, leaflet_map) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      observeEvent(leaflet_map(), {
        output$input_map <- renderLeaflet(leaflet_map())
      })
    }
  )
}  

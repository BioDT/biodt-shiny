box::use(
  shiny[NS, moduleServer, tags],
  bslib[card, card_header, card_body],
  leaflet[leafletOutput, renderLeaflet, leaflet, addTiles, setView, addMarkers]
)

#' @export
mod_grassland_inputmap_ui <- function(id) {
  ns <- NS(id)
  card(
    class = "ms-md-3 card-shadow",
    id = ns("input_map"),
    full_screen = TRUE,
    card_header(
      tags$h5("Input Map")
    ),
    card_body(
      leaflet::leafletOutput(
        ns("input_map_plot")
      )
    )
  )
}

#' @export
mod_grassland_inputmap_server <- function(id, r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      output$input_map_plot <- leaflet::renderLeaflet({
        leaflet::leaflet() |>
          leaflet::addTiles() |>
          leaflet::setView(lng = 11.8787,
                           lat = 51.3919,
                           zoom = 9) |>
          leaflet::addMarkers(lng = 11.8787,
                              lat = 51.3919)
      })
    }
  )
}  

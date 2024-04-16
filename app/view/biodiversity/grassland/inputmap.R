box::use(
  shiny[NS, moduleServer, tags],
  bslib[card, card_header, card_body],
  leaflet[setView, leaflet, leafletOptions, leafletOutput, renderLeaflet, addTiles],
  htmlwidgets[JS]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  card(
    id = ns("inputmap"),
    class = "ms-md-3 card-shadow",
    full_screen = TRUE,
    card_header(
      tags$h5("Input Map")
    ),
    card_body(      
      leafletOutput(
        ns("leaflet_output")
      )
    )
  )  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$leaflet_output <- renderLeaflet({      
      leaflet(
        options = leafletOptions(
          zoomControl = TRUE,
          minZoom = 2,
        )
      ) |>
      addTiles() |>
      setView(
        lng = 11.8787,
        lat = 51.3919,
        zoom = 9
      )
    })   
  })
}  

box::use(
  shiny[NS, moduleServer, tags],
  bslib[card, card_header, card_body],
  leaflet[leaflet, leafletOptions, leafletOutput, renderLeaflet, addEasyButton, easyButton, addTiles],
  htmlwidgets[JS]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  card(
    class = "ms-md-3 card-shadow",
    full_screen = TRUE,
    card_header(
      tags$h5("Input Map")
    ),
    card_body(
      id = ns("leaflet_output_cardbody"),
      leafletOutput(
        ns("map")
      )
    )
  )  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$map <- renderLeaflet({      
      leaflet(
        options = leafletOptions(
          zoomControl = TRUE,
          minZoom = 2,
        )
      ) |>
      addTiles() |>
      addEasyButton(
        easyButton(
          position = "topleft",
          icon = "ion-arrow-shrink",
          title = "Reset location",
          onClick = JS("function(btn, map) {map.setView(map._initialCenter, map._initialZoom);}")
        )
      )
    })   
  })
}  

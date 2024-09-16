box::use(
  shiny[NS, moduleServer, tags],
  bslib[card, card_header, card_body],
  leaflet[setView, leaflet, leafletOptions, leafletOutput, renderLeaflet, addTiles],
)

#' @export
disease_map_ui <- function(id, i18n) {
  ns <- NS(id)
  card(
    id = ns("map_wrapper"),
    class = "ms-md-3 card-shadow mt-2",
    full_screen = TRUE,
    card_header(
      tags$h2(
        class = "card_title",
        i18n$translate("Map")
      )
    ),
    card_body(
      leafletOutput(
        ns("map_output")
      )
    )
  )
}

#' @export
disease_map_server <- function(id, map, map_switch) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$map_output <- renderLeaflet({
      leaflet(
        options = leafletOptions(
          zoomControl = TRUE,
          min_zoom = 3
        )
      ) |>
        addTiles() |>
        setView(
          lng = 12.0,
          lat = 51.0,
          zoom = 10
        )
    })
  })
}
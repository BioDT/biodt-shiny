box::use(
  shiny[NS, moduleServer, tags],
  bslib[card, card_header, card_body],
  leaflet[setView, leaflet, leafletOptions, leafletOutput, renderLeaflet, addTiles],
)

#' @export
disease_map_ui <- function(id, i18n) {
  ns <- NS(id)
  card(
    id = ns("map"),
    class = "ms-md-3 card-shadow mt-2",
    full_screen = TRUE,
    card_header(
      tags$h2(
        class = "card_title",
        i18n$translate("Map")
      )
    ),
    card_body()
  )
}

#' @export
disease_map_server <- function(id, coordinates) {
  moduleServer(id, function(input, output, session) {

  })
}
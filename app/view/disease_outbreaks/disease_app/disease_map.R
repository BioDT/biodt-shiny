box::use(
  shiny[NS, moduleServer, tags, observeEvent],
  bslib[card, card_header, card_body],
  leaflet[setView, leaflet, leafletOptions, leafletOutput, renderLeaflet, addTiles],
)

box::use(
  app/logic/honeybee/honeybee_beekeeper_map[read_honeybee_tif, honeybee_leaflet_map]
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
disease_map_server <- function(id, map, map_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(map_selected(), {
        print(map_selected())
    })

  })
}
box::use(
  shiny[NS, moduleServer, tags, observeEvent, reactive, actionButton, reactiveVal, checkboxInput, updateCheckboxInput],
  bslib[card, card_header, card_body],
  leaflet[setView, leaflet, leafletOptions, leafletOutput, renderLeaflet, addTiles, leafletProxy, addRasterImage, clearImages],
  terra[rast, project],
  waiter[Waiter],
  app/logic/waiter[waiter_text]
)

box::use(
  app/logic/disease_outbreaks/disease_leaflet_map[read_and_project_raster, disease_leaflet_map]
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
      ),
    ),
  )
}

#' @export
disease_map_server <- function(id, leaflet_map) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(leaflet_map(), {
      req(leaflet_map())
      output_map <- leaflet_map()
      output$map_output <- renderLeaflet(output_map)
    })

  })
}
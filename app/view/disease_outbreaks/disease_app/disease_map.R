box::use(
  shiny[NS, moduleServer, tags, observeEvent, reactive, actionButton],
  bslib[card, card_header, card_body],
  leaflet[setView, leaflet, leafletOptions, leafletOutput, renderLeaflet, addTiles],
)

box::use(
  app/logic/disease_outbreaks/disease_leaflet_map[read_disease_outbreak_raster, make_full_tif_map_path, disease_outbreak_leaflet_map]
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
      actionButton(ns("runMap"), "Click here", )
    )
  )
}

#' @export
disease_map_server <- function(id, map_selected, disease_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    events <- reactive({
      disease_selected()
      map_selected()
    })

    observeEvent(events(),
      ignoreInit = TRUE,
    {
      if (disease_selected()) {
        print(map_selected())
      }
    })

  })
}
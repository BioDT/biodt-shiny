box::use(
  shiny[NS, moduleServer, tags, observeEvent, reactive, actionButton, reactiveVal, checkboxInput],
  bslib[card, card_header, card_body],
  leaflet[setView, leaflet, leafletOptions, leafletOutput, renderLeaflet, addTiles, leafletProxy, addRasterImage, clearImages],
  terra[rast, project]
)

box::use(
  app/logic/disease_outbreaks/disease_leaflet_map[read_and_project_raster, add_map_layer]
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
disease_map_server <- function(id, tab_disease_selected, map_filename) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tif_map_path <- ""
    tif_map_ok_prjct <- reactiveVal()

    events <- reactive({
      tab_disease_selected()
      # population_raster_selected()
      # outfirst_infection()
      map_filename()
    })

    leaflet_map <- leaflet() |>
      addTiles() |>
      setView(
          lng = 11.8787,
          lat = 51.3919,
          zoom = 4
      )

    output$map_output <- renderLeaflet(leaflet_map)

    observeEvent(events(),
      ignoreInit = TRUE,
    {      
      tif_map_projected <- read_and_project_raster(map_filename())

      #print(tif_map_projected)

      add_map_layer("map_output", tif_map_projected, 0.6)

        # TODO ----
        # 2. disease_outbreak_leaflet_map rozdelit na min. dve funkce, ktere budou brat jako 1. arg mapu, jako druhy "vrstvu mozaic" - ano/ne
        # 3. -""- - vrstvu infection - ano/ne
        # oboji pomoci leaflet proxy
        # map_output <- disease_outbreak_leaflet_map("map_output", tif_map_path)      
    })

    # TODO LAST - call the remove_map_layer from this module

  })
}
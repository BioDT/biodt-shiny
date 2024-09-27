box::use(
  shiny[NS, moduleServer, tags, observeEvent, reactive, actionButton, reactiveVal],
  bslib[card, card_header, card_body],
  leaflet[setView, leaflet, leafletOptions, leafletOutput, renderLeaflet, addTiles],
)

box::use(
  app/logic/disease_outbreaks/disease_leaflet_map[read_disease_outbreak_raster]
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
    )
  )
}

#' @export
disease_map_server <- function(id, map_selected, disease_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tif_map_path <- ""
    tif_map_ok_prjct <- reactiveVal()

    events <- reactive({
      disease_selected()
      map_selected()
    })

    # TODO ----
    # 1. incializovat mapu zde, pred observe
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
      if (disease_selected()) {
        # print(map_selected())
        # print(disease_selected())

        # which one of the two tif maps is going to be selected
        tif_map_path <- paste0("app/data/disease_outbreak/", map_selected()(), ".tif")         
        print(tif_map_path)

        # TODO ----
        # 2. disease_outbreak_leaflet_map rozdelit na min. dve funkce, ktere budou brat jako 1. arg mapu, jako druhy "vrstvu mozaic" - ano/ne
        # 3. -""- - vrstvu infection - ano/ne
        # oboji pomoci leaflet proxy
        # map_output <- disease_outbreak_leaflet_map("map_output", tif_map_path)
      }
    })

  })
}
box::use(
  shiny[NS, moduleServer, tags, observeEvent, reactive, actionButton, reactiveVal, checkboxInput, updateCheckboxInput],
  bslib[card, card_header, card_body],
  leaflet[setView, leaflet, leafletOptions, leafletOutput, renderLeaflet, addTiles, leafletProxy, addRasterImage, clearImages],
  terra[rast, project],
  waiter[Waiter],
  app/logic/waiter[waiter_text]
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
    actionButton(
      ns("clearAllLayers"),
      label = "clear layers",
      width = "200px"
    )
  )
}

#' @export
disease_map_server <- function(id, tab_disease_selected, map_filename) {
  moduleServer(id, function(input, output, session) {
    msg <- 
      waiter_text(message = tags$h3("Loading...",
                                    style = "color: #414f2f;"
      ))
    
    w <- Waiter$new(
      html = msg,
      color = "rgba(256,256,256,0.9)"
    )
    
    ns <- session$ns

    tif_map_path <- ""
    tif_map_ok_prjct <- reactiveVal()

    events <- reactive({
      tab_disease_selected()
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
        w$show()
        tif_map_projected <- read_and_project_raster(map_filename())

        add_map_layer("map_output", tif_map_projected, 0.9)
        w$hide()
      }
    )

    observeEvent(input$clearAllLayers,
      ignoreInit = TRUE,
      {
        leafletProxy("map_output") |>
          clearImages()
      }
    )
  })
}
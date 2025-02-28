box::use(
  shiny[NS, moduleServer, tags, observeEvent],
  bslib[card, card_header, card_body],
  leaflet[setView, leaflet, leafletOptions, leafletOutput, renderLeaflet, addTiles],
  htmlwidgets[JS],
  waiter[Waiter]
)

box::use(
  app/logic/grassland/grassland_update_inputmap[grassland_update_map],
  app/logic/waiter[waiter_text],
)

#' @export
grassland_dynamics_inputmap_ui <- function(id, i18n) {
  ns <- NS(id)
  card(
    id = ns("inputmap"),
    class = "ms-md-3 card-shadow mt-2",
    full_screen = TRUE,
    card_header(
      tags$h2(
        class = "card_title",
        i18n$translate("Input map"))
    ),
    card_body(
      leafletOutput(
        ns("leaflet_output")
      )
    )
  )
}

#' @export
grassland_dynamics_inputmap_server <- function(id, coordinates, tab_grassland_selected) { # nolint: object_length_linter.
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Define waiter ----
    msg <- waiter_text(message = tags$h3("Loading...",
        style = "color: #414f2f;"
      ))
    w <- Waiter$new(
      id = ns("inputmap"),
      html = msg,
      color = "rgba(256,256,256,0.9)",
    )

    observeEvent(tab_grassland_selected(),
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      {
        w$show()
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
      
        # Calls update inputmap (leafletProxy fn) with the given coordinates (lng/lat or DEIMS.id)----
        observeEvent(
        coordinates(),
        ignoreInit = TRUE,
        ignoreNULL = TRUE,
          {
            grassland_update_map(
              ns("leaflet_output"),
              coordinates()
            )
          }
        )
      
        w$hide()
      }
    )


    })
}

box::use(
  shiny[moduleServer, NS, tagList, checkboxInput, bootstrapPage, tags, observeEvent, reactiveVal, reactive, actionButton],
  bslib[card, card_header, card_body],
  leaflet[clearImages, leafletProxy]
)

box::use(
  app/logic/disease_outbreaks/disease_leaflet_map[remove_map_layer]
)

#' @export
disease_select_ui <- function(id, theme, i18n) {
  ns <- NS(id)
  bootstrapPage(
    theme = theme,
    card(
      class = "me-md-3 card-shadow overflow-hidden mt-2",
      title = "select_map",
      full_screen = FALSE,
      card_header(
        tags$h2(
          class = "card_title",
          "Show layer(s):"
        )
      ),
      card_body(
        checkboxInput(
          ns("mosaic_final"),
          label = "Population in whole Europe ('Mosaic_final.tif')",
          value = FALSE
        ),
        checkboxInput(
          ns("outfirst_infection"),
          label = "Outfirst infection ('outfirst_infection.tif')",
          value = FALSE
        ),
      ),
      actionButton(
        ns("clearAllLayers"),
        label = "clear layers"
      )
    )
  )
}

#' @export
disease_select_server <- function(id, tab_disease_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    out <- reactiveVal(NULL)

    events <- reactive({
      tab_disease_selected()
      input$mosaic_final
      input$outfirst_infection
    })

    observeEvent(
      events(),
      ignoreInit = TRUE,
      ignoreNULL = FALSE,
    {        
        if (input$mosaic_final == TRUE) {
          "Mosaic_final" |>
            out()
        }

        if (input$outfirst_infection == TRUE) {
          "outfirst_infection" |>
            out()
        }
    })

    observeEvent(
      input$clearAllLayers,
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        leafletProxy("map_output") |>
          clearImages()
      }
    )

    reactive(out())
  })
}
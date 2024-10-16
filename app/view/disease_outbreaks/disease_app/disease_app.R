box::use(
  shiny[NS, tagList, moduleServer, tags, reactiveVal, reactive, observeEvent],
  bslib[layout_column_wrap],
  htmltools[css],
)

box::use(
  app/view/disease_outbreaks/disease_app/disease_map[disease_map_ui, disease_map_server],
  app/view/disease_outbreaks/disease_app/disease_select[disease_select_ui, disease_select_server],
  app/logic/disease_outbreaks/disease_leaflet_map[read_and_project_raster, disease_leaflet_map]
)


#' @export
disease_app_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = NULL,
      fill = FALSE,
      style = css(grid_template_columns = "3fr 1fr"),
      disease_map_ui(
        ns("disease_map"), i18n
      ),
      disease_select_ui(
        ns("disease_select"), i18n
      ),
    ),
    tags$h3(i18n$translate("Output Plot"))
  )
}

#' @export
disease_app_server <- function(id, tab_disease_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Variables ----
    map <- reactiveVal()
    leaflet_map <- reactiveVal(NULL)

    observeEvent(tab_disease_selected(),
      ignoreInit = TRUE,
      {        
        "app/data/disease_outbreak/Mosaic_final.tif" |>
          read_and_project_raster() |>
          map()

        map() |>
          disease_leaflet_map(
            add_control = TRUE,
            main_map_features = TRUE,
          )  |>
          leaflet_map()        
      }
    )   

    # MAP itself ----
    disease_map_server(
      "disease_map",
      leaflet_map = leaflet_map,
    )
  })
}

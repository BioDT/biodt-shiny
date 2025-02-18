box::use(
  shiny[NS, tagList, moduleServer, tags, reactiveVal, observeEvent],
  bslib[layout_column_wrap],
  htmltools[css],
  waiter[Waiter],
)

box::use(
  app / view / disease_outbreaks / disease_app / disease_map[disease_map_ui, disease_map_server],
  app / view / disease_outbreaks / disease_app / disease_choose_file[disease_choose_file_ui, disease_choose_file_server],
  app / logic / disease_outbreaks / disease_leaflet_map[read_and_project_raster, disease_leaflet_map_basic],
  app / logic / waiter[waiter_text],
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
      disease_choose_file_ui(
        ns("disease_select"), i18n
      ),
    ),
    tags$h3(i18n$translate("Output Plot"))
  )
}

#' @export
disease_app_server <- function(id, tab_disease_selected) {
  moduleServer(id, function(input, output, session) {
    # Define waiter ----
    msg <- waiter_text(message = tags$h3("Loading data...",
      style = "color: #414f2f;"
    ))
    w <- Waiter$new(
      html = msg,
      color = "rgba(256,256,256,0.9)"
    )

    ns <- session$ns
    # Variables ----
    map <- reactiveVal()
    leaflet_map <- reactiveVal()

    new_tif_upload <- disease_choose_file_server("disease_select", tab_disease_selected())

    observeEvent(tab_disease_selected(),
      ignoreInit = TRUE,
      {
        w$show()
        "app/data/disease_outbreak/Mosaic_final.tif" |>
          read_and_project_raster() |>
          map()

        map() |>
          disease_leaflet_map_basic(
            add_control = TRUE,
            main_map_features = TRUE
          ) |>
          leaflet_map()

        w$hide()

      }
    )

    disease_map_server(
      "disease_map",
      map_original = map,
      leaflet_map = leaflet_map,
      new_tif_upload = new_tif_upload
    )
  })
}

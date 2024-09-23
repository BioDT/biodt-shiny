box::use(
  shiny[NS, tagList, moduleServer, tags, reactiveVal],
  bslib[layout_column_wrap],
  htmltools[css],
)

box::use(
  app/view/disease_outbreaks/disease_app/disease_map[disease_map_ui, disease_map_server],
  app/view/disease_outbreaks/disease_app/disease_select[disease_select_ui, disease_select_server],
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
      # tags$h3(i18n$translate("Location")),
    ),
    tags$h3(i18n$translate("Output Plot"))
  )
}

#' @export
disease_app_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Variables ----
    tif_map <- reactiveVal(NULL)
    tif_map_path <- reactiveVal(NULL)

    # selection of raster map
    disease_select_server("disease_select")
    # print(tif_map_name)

    # MAP itself ----
    disease_map_server("disease_map", map_selected = tif_map())
  })
}

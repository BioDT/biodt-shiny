box::use(
  shiny[moduleServer, NS, tagList, tags, actionButton, observeEvent, reactiveVal, bootstrapPage],
  leaflet[leaflet, addTiles],
  bslib[layout_column_wrap],
)

box::use(
  app/view/honeybee/beekeeper_control[beekeeper_control_ui],
  app/view/honeybee/honeybee_map[honeybee_map_ui, honeybee_map_server],
  app/view/honeybee/honeybee_param[honeybee_param_ui, honeybee_param_server],
  app/view/honeybee/honeybee_lookup[honeybee_lookup_ui, honeybee_lookup_server],
  app/view/echarty[echarty_ui, echarty_server],
  app/logic/honeybee/honeybee_beekeeper_map[read_honeybee_tif, honeybee_leaflet_map],
  app/logic/honeybee/honeybee_beekeeper_plot[honeybee_beekeeper_plot]
)

#' @export
honeybee_beekeeper_ui <- function(id,
                                  theme) {
  ns <- NS(id)
  tagList(
    # actionButton(ns("change"), "change"),
    tags$div(
      beekeeper_control_ui(ns("beekeeper_control")),
      layout_column_wrap(
        width = NULL,
        fill = FALSE,
        style = htmltools::css(grid_template_columns = "2fr 1fr"),
        honeybee_map_ui(ns("beekeeper_map")),
        honeybee_param_ui(ns("beekeeper_param"),
                          theme)
      ),
      honeybee_lookup_ui(ns("beekeeper_lookup")),
      echarty_ui(ns("beekeeper_plot"))
    )
  )
}

#' @export
honeybee_beekeeper_server <- function(id,
                                      r) {
  moduleServer(id, function(input, output, session) {
    
    # Map ----
    map <- reactiveVal()
    # Hardcoded for prototype
    "app/data/honeybee/map.tif" |>
      read_honeybee_tif() |>
      honeybee_leaflet_map() |>
      map()
    
    coordinates <- honeybee_map_server("beekeeper_map",
      leaflet_map = map
    )
    
    observeEvent(
      coordinates(),
      print(coordinates())
    )
    # Parameters ----
    
    parameters <- honeybee_param_server("beekeeper_param")
    
    observeEvent(
      parameters(),
      print(parameters())
    )
    # Lookup table ----
    lookup_input <- reactiveVal()
    # Hardcoded for prototype
    "app/data/honeybee/lookup_table.csv" |>  
      readr::read_csv(show_col_types = FALSE) |>
      lookup_input()
    
    lookup <- honeybee_lookup_server("beekeeper_lookup",
                                     lookup_table = lookup_input)
    
    observeEvent(
      lookup(),
      print(lookup())
    )
    
    # Plot ----
    plot <- reactiveVal()
    
    # Hardcoded for prototype
    honeybee_beekeeper_plot(
      "app/data/honeybee/output_example/Result_table_original.csv",
      "app/data/honeybee/output_example/weather_412.txt"
    ) |>
      plot()
    
    echarty_server("beekeeper_plot",
                   echarty_plot = plot)
    

  })
}
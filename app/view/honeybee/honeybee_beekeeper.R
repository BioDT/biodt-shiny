box::use(
  shiny[moduleServer, NS, tagList, tags, actionButton, observeEvent, reactiveVal, bootstrapPage, req],
  leaflet[leaflet, addTiles],
  bslib[layout_column_wrap],
  htmltools[css],
  waiter[Waiter],
)

box::use(
  app/view/honeybee/beekeeper_control[beekeeper_control_ui, beekeeper_control_server],
  app/view/honeybee/honeybee_map[honeybee_map_ui, honeybee_map_server],
  app/view/honeybee/honeybee_param[honeybee_param_ui, honeybee_param_server],
  app/view/honeybee/honeybee_lookup[honeybee_lookup_ui, honeybee_lookup_server],
  app/view/echarty[echarty_ui, echarty_server],
  app/logic/honeybee/honeybee_beekeeper_map[read_honeybee_tif, honeybee_leaflet_map],
  app/logic/honeybee/honeybee_beekeeper_plot[honeybee_beekeeper_plot],
  app/logic/waiter[waiter_text],
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
        style = css(grid_template_columns = "2fr 1fr"),
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
                                      r,
                                      beekeeper_selected) {
  moduleServer(id, function(input, output, session) {
    
    # Define waiter ----
    msg <- list(waiter_text(message = tags$h3("Loding data...",
                                              style = "color: #414f2f;")),
                waiter_text(message = tags$h3("Computing Beehave simulation...",
                                              style = "color: #414f2f;")))
    
    w <- Waiter$new(html = msg[[1]],
                    color = "rgba(256,256,256,0.9)")
    
    # Variables ----
    map <- reactiveVal()
    lookup_table <- reactiveVal()
    plot <- reactiveVal()
    
    # Initialization ----
    observeEvent(beekeeper_selected(),
                 ignoreInit = TRUE,
                 ignoreNULL = TRUE,
                 {
                   req(beekeeper_selected(),
                       is.null(map()) |
                       is.null(lookup_table()) |
                       is.null(plot()))
                   w$show()
                   # Load map
                   # Hardcoded for prototype
                   "app/data/honeybee/map.tif" |>
                     read_honeybee_tif() |>
                     honeybee_leaflet_map() |>
                     map()
                   
                   # Lookup table
                   # Hardcoded for prototype
                   "app/data/honeybee/lookup_table.csv" |>  
                     readr::read_csv(show_col_types = FALSE) |>
                     lookup_table()
                   
                   # Hardcoded for prototype
                   honeybee_beekeeper_plot(
                     "app/data/honeybee/output_example/Result_table_original.csv",
                     "app/data/honeybee/output_example/weather_412.txt"
                   ) |>
                     plot()
                   
                   w$hide()
                 })
    
    # Map ----
    coordinates <- honeybee_map_server("beekeeper_map",
      leaflet_map = map
    )

    # Parameters ----
    parameters <- honeybee_param_server("beekeeper_param")

    # Lookup table ----
    lookup <- honeybee_lookup_server("beekeeper_lookup",
                                     lookup_table = lookup_table)
    
    # Plot ----
    echarty_server("beekeeper_plot",
                   echarty_plot = plot)
    
    # Execution ----
    beekeeper_control_server("beekeeper_control",
                             coordinates,
                             lookup,
                             parameters,
                             w)
  })
}

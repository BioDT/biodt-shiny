box::use(
  shiny[NS, moduleServer, icon, tags, textOutput],
  bslib[card, card_header, card_body, value_box, layout_columns],
  waiter[Waiter],
)

#' @export
grassland_dynamics_soil_main_values_ui <- function(
  id,
  i18n
) {
  ns <- NS(id)
  card(
    id = "soil_main_values",
    class = "mx-md-3 card-shadow mb-2",
    card_body(
      layout_columns(
        fill = FALSE,
        value_box(
          title = textOutput("silt"),
          value = textOutput("silt_val"),
          showcase = icon("layer-group"),
          theme = "primary"
        ),
        value_box(
          title = textOutput("clay"),
          value = textOutput("clay_val"),
          showcase = icon("layer-group"),
          theme = "secondary"
        ),
        value_box(
          title = textOutput("sand"),
          value = textOutput("sand_val"),
          showcase = icon("layer-group"),
          theme = "success"
        )
      )
    )
  )
}

#' @export
grassland_dynamics_soil_main_values_server <- function(
    id,
    main_values) {
  moduleServer(id, function(input, output, session) {
    main_values_reactive <- reactiveVal()

    data_table_reactive(main_values_reactive)


    
  })
}

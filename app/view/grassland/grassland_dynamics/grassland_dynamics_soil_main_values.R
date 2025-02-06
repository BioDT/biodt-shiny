box::use(
  shiny[NS, moduleServer, icon, tags, textOutput],
  bslib[card, card_header, card_body, value_box],
  waiter[Waiter],
)

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
          theme = "pink"
        )
      )
    )
  )
}
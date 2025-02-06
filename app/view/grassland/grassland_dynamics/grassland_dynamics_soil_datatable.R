box::use(
  shiny[NS, moduleServer, tags, reactiveVal],
  bslib[card, card_header, card_body],
  waiter[Waiter],
  DT[DTOutput, renderDT],
)

#' @export
grassland_dynamics_soil_datatable_ui <- function(
  id,
  i18n
) {
  ns <- NS(id)
  card(
    id = ns("datatable"),
    class = "mx-md-3 card-shadow mb-2",
    card_header(
      tags$h2(
        class = "card_title",
        i18n$translate("Soil")
      )
    ),
    card_body(
      tags$div(
        id = ns("soil_data_table_wrap"),
        DTOutput(
          ns("soil_data_table")
        )
      )
    )
  )
}

#' @export
grassland_dynamics_soil_datatable_server <- function(id,
                                                      data_table) {
  moduleServer(id, function(input, output, session) {
    data_table_reactive <- reactiveVal()

    data_table_reactive(data_table)

    output$soil_data_table <- renderDT(
      data_table_reactive(),
    )

  })
}

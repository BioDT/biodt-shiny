box::use(
  shiny[NS, moduleServer, tags, reactiveVal, observeEvent],
  bslib[card, card_header, card_body],
  waiter[Waiter],
  DT[DTOutput, renderDT],
  dplyr[select]
)

box::use(
  app/logic/waiter[waiter_text],
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
grassland_dynamics_soil_datatable_server <- function(id, data_table, tab_grassland_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Define waiter ----
    msg <- waiter_text(message = tags$h3("Loading...",
        style = "color: #414f2f;"
      ))
    w <- Waiter$new(
      id = ns("datatable"),
      html = msg,
      color = "rgba(256,256,256,0.9)",
    )

    observeEvent(
      tab_grassland_selected(),
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      {
        w$show()
        output$soil_data_table <- renderDT(
          rownames = FALSE, 
          {
            data_table
          }
        )
        w$hide()
      }
    )
  })
}

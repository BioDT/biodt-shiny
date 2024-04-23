box::use(
  shiny[moduleServer, NS, tagList, tags, reactive, reactiveVal, observeEvent],
  bslib[card, card_header, card_body],
  DT[DTOutput, renderDT],
)

#' @export
honeybee_lookup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Lookup Table----
    card(
      class = "mx-md-3 card-shadow",
      title = "lookup_table",
      full_screen = TRUE,
      card_header(
        tags$h5("Lookup Table"),
      ),
      card_body(
        tags$div(
          DTOutput(
            ns("lookup_table")
          )
        )
      )
    )
  )
}

#' @export
honeybee_lookup_server <- function(id,
                                   lookup_table) {
  moduleServer(id, function(input, output, session) {
    lookup_table_react <- reactiveVal()

    # Update local lookup_table_react based on input lookup_table
    observeEvent(lookup_table(), {
      lookup_table_react(lookup_table())
    })

    output$lookup_table <- renderDT(lookup_table(),
      editable = list(target = "cell", disable = list(columns = c(0, 1))),
      selection = "none",
      style = "bootstrap5",
      options = list(
        scrollX = TRUE,
        paging = FALSE,
        searching = FALSE,
        info = FALSE
      ),
      class = c("hover", "compact")
    )

    # Lookup table edit logic ----
    observeEvent(input$lookup_table_cell_edit, {
      temp <- lookup_table_react()
      temp[
        input$lookup_table_cell_edit$row,
        input$lookup_table_cell_edit$col
      ] <-
        input$lookup_table_cell_edit$value
      lookup_table_react(temp)
    })

    reactive(lookup_table_react())
  })
}

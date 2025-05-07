box::use(
  shiny[NS, moduleServer, tags, reactiveVal, observeEvent, checkboxInput, uiOutput, renderUI, req],
  bslib[card, card_header, card_body],
  waiter[Waiter],
  DT[DTOutput, renderDT, datatable],
  dplyr[select]
)

box::use(
  app / logic / waiter[waiter_text],
)

#' @export
grassland_dynamics_manage_datatable_ui <- function(
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
        i18n$translate("Grassland Management Actions")
      ),
      # checkboxInput(ns("show_mngmntdata"), "Show Management Data Table", value = FALSE, width = "200px")
    ),
    card_body(
      tags$div(
        id = ns("mngmnt_data_table_wrap"),
        DTOutput(
          ns("mngmnt_data_table")
        )
      )
    )
  )
}

#' @export
grassland_dynamics_manage_datatable_server <- function(id, data_table, tab_grassland_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_table_reactive <- reactiveVal()
    data_table_reactive(data_table)

    # Define waiter ----
    msg <- waiter_text(message = tags$h3("Loading...", style = "color: #414f2f;"))
    w <- Waiter$new(
      id = ns("mngmnt_data_table_wrap"),
      html = msg,
      color = "rgba(256,256,256,0.9)",
    )

    # show_mngmnttable <- reactiveVal(FALSE)
    # div_table_wrap_tag <- tags$div(
    #   id = "div_table_wrap",
    #   DTOutput(
    #     ns("mngmnt_data_table")
    #   )
    # )

    # observeEvent(input$show_mngmntdata, {
    #   if (input$show_mngmntdata == TRUE) {
    #     show_mngmnttable(TRUE)
    #   } else {
    #     show_mngmnttable(FALSE)
    #   }
    # })

    # observeEvent(show_mngmnttable(), ignoreInit = TRUE, {
    #   req(data_table)
    #   if (show_mngmnttable() == FALSE) {
    #     output$mngmnt_data_table_wrap <- renderUI(NULL)
    #   }
    #   if (show_mngmnttable() == TRUE) {
    #     print(div_table_wrap_tag)
    #     output$mngmnt_data_table_wrap <- renderUI({
    #       div_table_wrap_tag
    #     })
    #   }
    # })

    observeEvent(
      tab_grassland_selected(),
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      {
        w$show()
        output$mngmnt_data_table <- renderDT(
          data_table_reactive(),
          editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 4, 5, 6))),
          selection = "none",
          style = "auto",
          options = list(
            scrollX = TRUE,
            paging = TRUE,
            searching = TRUE,
            info = FALSE
          ),
          class = paste('cell-border stripe compact'),
          fillContainer = FALSE,
          rownames = FALSE
        )
        w$hide()
      }
    )
  })
}

box::use(
  shiny[NS, moduleServer, tags, reactiveVal, observeEvent, checkboxInput, uiOutput, renderUI, req, actionButton, HTML],
  bslib[card, card_header, card_body],
  waiter[Waiter],
  DT[DTOutput, renderDT, datatable],
  dplyr[select]
)

box::use(
  app / logic / waiter[waiter_text],
)

#' @export
grassland_dynamics_soil_datatable_ui <- function(
    id,
    i18n) {
  ns <- NS(id)
  card(
    id = ns("datatable"),
    class = "mx-md-3 card-shadow mb-2",
    card_header(
      tags$div(
        class = "d-flex justify-content-between align-items-center",
        tags$h2(
          class = "card_title",
          i18n$translate("Soil Data")
        ),
        uiOutput(ns("show_soildata_btn")),
      ),
    ),
    card_body(
      uiOutput(ns("soil_data_table_wrap"))
    )
  )
}

#' @export
grassland_dynamics_soil_datatable_server <- function(id, data_table, tab_grassland_selected, i18n) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Define waiter ----
    msg <- waiter_text(message = tags$h3("Loading...", style = "color: #414f2f;"))
    w <- Waiter$new(
      id = ns("datatable"),
      html = msg,
      color = "rgba(256,256,256,0.9)",
    )

    show_soiltable <- reactiveVal(FALSE)
    div_table_wrap_tag <- tags$div(
      id = "div_table_wrap",
      DTOutput(
        ns("soil_data_table")
      )
    )

    output$show_soildata_btn <- renderUI({
      icon_dir <- if (show_soiltable()) "up" else "down"
      icon_html <- sprintf('<i class="fa-solid fa-arrow-%s"></i>', icon_dir)
      actionButton(
        ns("show_soildata"),
        HTML(icon_html),
        class = "primary-button",
        `aria-expanded` = tolower(as.character(show_soiltable())),
        title = ifelse(show_soiltable(), i18n$t("Collapse soil data table"), i18n$t("Expand soil data table"))
      )
    })

    observeEvent(input$show_soildata, {
      show_soiltable(!show_soiltable())
    })

    observeEvent(show_soiltable(), ignoreInit = TRUE, {
      req(data_table)
      if (show_soiltable() == FALSE) {
        output$soil_data_table_wrap <- renderUI(NULL)
      }
      if (show_soiltable() == TRUE) {
        print(div_table_wrap_tag)
        output$soil_data_table_wrap <- renderUI({
          div_table_wrap_tag
        })
      }
    })

    observeEvent(
      tab_grassland_selected(),
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      {
        w$show()
        req(div_table_wrap_tag)
        output$soil_data_table <- renderDT({
          datatable(
            data_table,
            class = paste("cell-border stripe compact"),
            style = "auto",
            fillContainer = FALSE,
            rownames = FALSE
          )
        })
        w$hide()
      }
    )
  })
}

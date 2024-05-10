box::use(
  shiny[moduleServer, NS, tagList, tags, reactive, reactiveVal, observeEvent],
  bslib[card, card_header, card_body],
  DT[DTOutput, renderDT],
  htmlwidgets[JS],

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
          id = "bee-lookup-table",
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
      class = c("hover", "compact"),
      callback = JS(paste0('
        let th_cells = document.querySelectorAll("#bee-lookup-table table thead tr th")

        const tooltipInfo = [
          "Amount of pollen g/m^2 per day",
          "Conversion factor (mol/l) from nectar (l) into sugar (mol)",
          "Amount of nectar l/m^2 per day",
          "Minimum time in seconds a bee needs to harvest nectar resources during one flight",
          "Minimum time in seconds a bee needs to harvest pollen resources during one flight",
          "Calendar day when resources become available",
          "Calendar day until resources are available"
        ]

        th_cells.forEach((el, idx) => {
          if (idx == 0 || idx == 1) {
            return;
          } else {
            /*
            let divWrap = document.createElement("div");
            divWrap.setAttribute("data-bs-title", "default link...");
            divWrap.setAttribute("data-bs-toggle", "tooltip");
            divWrap.setAttribute("class", "tooltip");

            let spanText = document.createElement("span");
            spanText.setAttribute("class", "tooltiptext");
            spanText.innerHTML = tooltipInfo[idx - 2];

            let spanIco = document.createElement("span");
            spanIco.setAttribute("class","fa fa-circle-info");          

            divWrap.appendChild(spanText)
            
            th_cells[idx].appendChild(spanIco)
            th_cells[idx].appendChild(divWrap)
            */

            // th_cells[idx].setAttribute("type", "button");
            // th_cells[idx].setAttribute("class", "btn btn-secondary");
            // th_cells[idx].setAttribute("data-bs-title", "default link...");
            // th_cells[idx].setAttribute("data-bs-content", "OBSAH...");
            // th_cells[idx].setAttribute("data-bs-toggle", "tooltip");
            
            
            let btnTip = document.createElement("button");
            btnTip.setAttribute("type", "button");
            btnTip.setAttribute("data-bs-toggle", "popover");
            btnTip.setAttribute("data-bs-content", "body content here");
            btnTip.setAttribute("title", tooltipInfo[idx - 2]);

            th_cells[idx].appendChild(btnTip)

            console.log("th_cells[idx]:::", th_cells[idx])
          }
        })
      '))
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

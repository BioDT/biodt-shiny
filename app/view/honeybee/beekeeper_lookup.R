box::use(
  shiny[moduleServer, NS, tagList, tags, reactive, reactiveVal, observeEvent, icon],
  bslib[card, card_header, card_body],
  DT[DTOutput, renderDT],
  htmlwidgets[JS],
)

#' @export
honeybee_lookup_ui <- function(id, i18n) {
  ns <- NS(id)
  tagList(
    # Lookup Table----
    card(
      class = "mx-md-3 card-shadow",
      title = "lookup_table",
      full_screen = TRUE,
      card_header(
        tags$h2(
          i18n$t("Lookup Table"),
          class = "card_title",
        ),
      ),
      card_body(
        tags$p(
          i18n$t("In the landscape surrounding the hive, floral resources are considered only in the fields and meadows, called 'food patches'. Each food patch is characterised by the metrics listed below (the area in m² is given and cannot be changed). Pollen and nectar quantities are based on estimates of quantity per flower, number of flowers per plant and number of plants per square metre. For simplicity, and in the absence of more detailed data, the daily supply of nectar and pollen provided by the plants was assumed to be constant throughout the flowering period.")
        ),
        tags$p(i18n$t("All values are based on previous studies (e.g. Horn et al. 2020, "), tags$a("https://doi.org/10.1002/eap.2216", href = "https://doi.org/10.1002/eap.2216", target = "_blank"), i18n$t(") or a best guesses.")),
        tags$p(
          i18n$t("The user is encouraged to use own estimates or experiment using alternative values. You can double click the value to enter edit mode."),
          tags$br(), i18n$t("Hover over the "), icon("circle-info"), i18n$t(" to get detailed description of the variables.")
        ),
        tags$div(
          id = "bee-lookup-table",
          DTOutput(
            ns("lookup_table")
          )
        ),
        tags$p(
          i18n$t("The lookup table allows to translate the land cover around a hive into floral resources (Nectar, Pollen) on a daily resolution. Therefore, the resources around a hive, e.g. grid cells of oil seed rape during the flowering period, will be summed up around a hive (square with an edge length of 3 km and the hive in the middle). Grid cells of the same land cover will be summarized into one polygon and split up if the polygon is too large. Please consult the scripts available on "), tags$a("https://github.com/BioDT", href = "https://github.com/BioDT/uc-pollinators/tree/main/scripts", target = "_blank"), i18n$t(" for more details."),
          class = "my-2",
          icon("circle-exclamation")
        )
      )
    )
  )
}

#' @export
honeybee_lookup_server <- function(id,
                                   lookup_table, i18n) {
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
        info = FALSE,
        language = list(
          zeroRecords = "∅",
          loadingRecords = "⌛",
          info = "[_START_; _END_] ⊂ max(_TOTAL_)",
          lengthMenu = "_MENU_",
          paginate = list("previous" = "⬅️", "next" = "➡️")
        )
      ),
      class = c("hover", "compact"),
      callback = JS(paste0(
        "
        let th_cells = document.querySelectorAll('#bee-lookup-table table thead tr th');

        const tooltipInfo = ['",
        i18n$t("Amount of pollen g/m^2 per day"), "', '",
        i18n$t("Conversion factor (mol/l) from nectar (l) into sugar (mol)"), "', '",
        i18n$t("Amount of nectar l/m^2 per day"), "', '",
        i18n$t("Minimum time in seconds a bee needs to harvest nectar resources during one flight"), "', '",
        i18n$t("Minimum time in seconds a bee needs to harvest pollen resources during one flight"), "', '",
        i18n$t("Calendar day when resources become available"), "', '",
        i18n$t("Calendar day until resources are available"), "'
        ];

        th_cells.forEach((el, idx) => {
          if (idx == 0 || idx == 1) {
            return;
          } else {
            let tooltipEl = document.createElement('i');
            tooltipEl.setAttribute('class', 'fas fa-circle-info fa-fw')
            tooltipEl.setAttribute('aria-label', 'circle-info icon')
            tooltipEl.setAttribute('type', 'button');
            tooltipEl.setAttribute('data-bs-toggle', 'popover');
            tooltipEl.setAttribute('title', tooltipInfo[idx - 2]);

            th_cells[idx].appendChild(tooltipEl)
          }
        })
      "
      ))
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

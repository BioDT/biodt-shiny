box::use(
  shiny[moduleServer, NS, bootstrapPage, tags, observeEvent, reactiveVal, reactive, fileInput],
  bslib[card, card_header, card_body],
)

#' @export
disease_choose_file_ui <- function(id, theme, i18n) {
  ns <- NS(id)
  bootstrapPage(
    theme = theme,
    card(
      class = "me-md-3 card-shadow overflow-hidden mt-2",
      title = "select_map",
      full_screen = FALSE,
      card_header(
        tags$h2(
          class = "card_title",
          "File input"
        )
      ),
      card_body(
        fileInput(ns("tif_file"), "Choose .tif file", accept = ".tif"),
      ),
    )
  )
}

#' @export
disease_choose_file_server <- function(id, tab_disease_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    out <- reactiveVal(NULL)

    observeEvent(
      input$tif_file,
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        out(input$tif_file$datapath)
      }
    )

    reactive(out())
  })
}
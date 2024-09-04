box::use(
  shiny[moduleServer, NS, tagList, selectInput, bootstrapPage]
)

disease_select_ui <- function(id, theme, i18n) {
  ns <- NS(id)
  bootstrapPage(
    theme = theme,
    card(
      class = "me-md-3 card-shadow overflow-hidden",
      title = "select_map",
      full_screen = FALSE,
      card_header(
        tags$h2(
          class = "card_title",
          "Disease Map Select"
        )
      ),
      card_body(
        selectInput(
          inputId = "disease_map_name",
          label = "The name of disease map:",
          choices = c("Mosaic_final", "outfirst_infection")
        )
      )
    )
  )
}

#' @export
disease_select_server <- function(id, map_switch) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  })
}
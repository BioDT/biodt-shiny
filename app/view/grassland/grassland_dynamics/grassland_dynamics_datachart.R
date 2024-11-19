box::use(
  shiny[NS, moduleServer, tags],
  bslib[card, card_header, card_body],
)

#' @export
grassland_dynamics_datachart_ui <- function(id, i18n) {
  ns <- NS(id)
  card(
    id = ns("datachart"),
    class = "mx-md-3 card-shadow mb-2",
    full_screen = TRUE,
    card_header(
      tags$h2(
        class = "card_title",
        i18n$translate("PFTs Chart"))
    ),
    card_body(     
    ),
  )
}

#' @export
grassland_dynamics_datachart_server <- function(id) { # nolint
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}

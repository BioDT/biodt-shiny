box::use(
  shiny[moduleServer, NS, tags],
  bslib[card, card_header, card_body],
)


#' @export
beekeeper_control_ui <- function(id, i18n) {
  ns <- NS(id)
  card(
    id = ns("control"),
    class = "mt-2 mx-md-3 card-shadow overflow-hidden",
    card_header(
      tags$div(
        class = "row d-flex justify-content-between align-items-center my-1",
        tags$div(
          class = "col-md-12 col-sm-12 me-auto",
          tags$h2(
            class = "card_title",
            i18n$translate("Honeybee Beekeeper Case")
          ),
        ),
      )
    ),
    card_body(
      id = ns("control"),
      tags$div(
        class = "row d-flex justify-content-between card-list",
        tags$h3(
          class = "pdt_instructions_title",
          "Instructions"
        ),
        tags$ol(
          tags$style("
            .card-list ol li {
              margin-bottom: 10px;
              margin-left: 20px;
            }
          "),
          tags$li(
            tags$b("Select"), "point on the map by clicking the",
            shiny::icon("location-dot"),
            tags$b("icon")
          ),
          tags$li(tags$b("Adjust"), "the parameters"),
          tags$li(tags$b("Change"), "the lookup table values if needed"),
          tags$li(
            "Click the",
            tags$b("Run simulation"),
            "button",
          ),
        ),
        tags$p("The simulation results can be seen in the output plot, select your experiment from the dropdown menu.")
      )
    )
  )
}

#' @export
beekeeper_control_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

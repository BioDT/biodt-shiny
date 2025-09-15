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
            i18n$translate("Honeybee Beekeeper Case"),
            class = "card_title"
          ),
        ),
      )
    ),
    card_body(
      id = ns("control"),
      tags$div(
        class = "row d-flex justify-content-between card-list",
        tags$h3(
          i18n$t("Instructions"),
          class = "pdt_instructions_title"
        ),
        tags$ol(
          tags$style(
            "
            .card-list ol li {
              margin-bottom: 10px;
              margin-left: 20px;
            }
          "
          ),
          tags$li(
            tags$b(i18n$t("Select")),
            i18n$t("point on the map by clicking the"),
            shiny::icon("location-dot"),
            tags$b(i18n$t("icon"))
          ),
          tags$li(tags$b(i18n$t("Adjust")), i18n$t("the parameters")),
          tags$li(tags$b(i18n$t("Change")), i18n$t("the lookup table values if needed")),
          tags$li(
            i18n$t("Click the"),
            tags$b(i18n$t("Run simulation")),
            i18n$t("button"),
          ),
        ),
        tags$p(i18n$t("The simulation results can be seen in the output plot, select your experiment from the dropdown menu.")),
        tags$p(
          i18n$t("Should you want to read more about the use case and learn how to use the pDT, you can refer to the relevant tutorial that can be found on the BioDT Training Platform "),
          tags$a(i18n$t("here"), href = "https://training.lifewatch.eu:9001/course/view.php?id=122", target = "_blank"),
          "."
        )
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

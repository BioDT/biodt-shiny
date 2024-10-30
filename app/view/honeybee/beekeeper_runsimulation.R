box::use(
  shiny[NS, moduleServer, tags, fluidRow, column, actionButton, reactiveVal, observeEvent],
  bslib[card, card_header, card_body],
  shinyjs[disabled, disable, enable],
  purrr[is_empty],
  waiter[Waiter],
)

box::use(
  app/logic/waiter[waiter_text],
)

#' @export
beekeeper_runsimulation_ui <- function(id, i18n) {
  ns <- NS(id)
  card(
    id = ns("runsimulation"),
    class = "mt-2 mx-md-3 card-shadow overflow-hidden",
    card_header(
      tags$div(
        class = "row d-flex justify-content-between align-items-center my-1",
        tags$div(
          class = "col-md-8 col-sm-12 me-auto",
          tags$h2(
            class = "card_title",
            i18n$translate("When you are finished, run your simulation by clicking the button:")),
        ),
        tags$div(
          class = "col-md-4 col-sm-12 d-flex flex-row justify-content-end",
          disabled(
            actionButton(
              ns("run_simulation"),
              label = i18n$translate("Run simulation"),
              width = "100%",
              class = "btn-secondary btn-lg",
              style = "max-width: 300px"
            )
          )
        )
      )
    ),
  )
}

#' @export
beekeeper_runsimulation_server <- function(
    id,
    coordinates,
    lookup,
    parameters,
    landuse_map,
    session_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Define waiter ---- TO ADD later

    # Prepare directory for results ----
    # Non-persistent data solution
    # Making a beekeeper dir in the shared folder
    temp_dir <- session_dir |>
      file.path("beekeeper")

    experiment_list <- reactiveVal(
      c(Example = "app/data/honeybee/output_example/Result_table_original.csv")
    )
    counter <- reactiveVal(0)

    # Run workflow button ----
    observeEvent(
      coordinates(),
      ignoreNULL = FALSE,
      {
        if (!is_empty(coordinates()) &
          !is_empty(lookup()) &
          !is_empty(parameters())) {
          enable("run_simulation")
        } else {
          disable("run_simulation")
        }
      }
    )


  })
}
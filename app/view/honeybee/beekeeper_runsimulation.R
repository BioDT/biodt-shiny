box::use(
  shiny[NS, moduleServer, tags, actionButton, observeEvent, req],
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
              class = "btn-secondary",
              style = "max-width: 200px"
            )
          )
        )
      )
    ),
  )
}
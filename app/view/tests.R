box::use(
  shiny[NS, fillPage, div, p, moduleServer, tagList, tags],
  bslib[card, card_header, card_body],
  config,
)

#' @export
mod_tests_ui <- function(id) {
  ns <- NS(id)

  env_active <- Sys.getenv("R_CONFIG_ACTIVE")
  print("Sys.getenv(`R_CONFIG_ACTIVE`):::")
  print("AKA what env is active? prod/dev?:::")
  print(Sys.getenv("R_CONFIG_ACTIVE"))

  ui_prod <- tags$div(
    class = "column",
    tags$div(
      class = "col",
      card(
        id = ns("env_heading"),
        class = "mt-2 mx-md-3 card-shadow",
        card_header(
          tags$div(
            class = "row d-flex justify-content-between align-items-center my-1",
            tags$div(
              class = "col-md-8 col-sm-12 me-auto",
              tags$h1("`prod` env is active"),
              tags$h2("(Check-up control tests & Environment values)"),
            ),
          )
        ),
        card_body(
          id = ns("env_values"),
          tags$div(
            class = "row d-flex justify-content-between",
            tags$p(
              "what env is active (prod/dev)?:::"
            ),
            tags$p(env_active),
          )
        )
      ),
    )
  )

  ui_dev <- tags$div(
    class = "column",
    tags$div(
      class = "col",
      card(
        id = ns("env_heading"),
        class = "mt-2 mx-md-3 card-shadow",
        card_header(
          tags$div(
            class = "row d-flex justify-content-between align-items-center my-1",
            tags$div(
              class = "col-md-8 col-sm-12 me-auto",
              tags$h1("`dev` env is active"),
              tags$h2("(Check-up control tests & Environment values)"),
            ),
          )
        ),
        card_body(
          id = ns("env_values"),
          tags$div(
            class = "row d-flex justify-content-between",
            tags$p(
              "what env is active (prod/dev)?:::"
            ),
            tags$p(env_active),
          )
        )
      ),
    )
  )

  if (env_active == "prod") {
    ui_prod
  } else {
    ui_dev
  }
}

#' @export
mod_tests_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    env_active <- Sys.getenv("R_CONFIG_ACTIVE")
    if (env_active == "") {
      # Handle unset or empty R_CONFIG_ACTIVE variable
      print("Env variable `R_CONFIG_ACTIVE` not set")
    } else {
      print('Sys.getenv("R_CONFIG_ACTIVE") ==\n')
      print(env_active)
      print("\n\\n\\n\\n\n")
    }

  })
}

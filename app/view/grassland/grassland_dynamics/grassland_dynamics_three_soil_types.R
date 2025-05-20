box::use(
  shiny[NS, moduleServer, icon, tags, textOutput, renderText, reactiveVal, observeEvent],
  bslib[card, card_header, card_body, value_box, layout_columns],
  waiter[Waiter],
)

box::use(
  app / logic / waiter[waiter_text],
)

#' @export
grassland_dynamics_three_soil_types_ui <- function(
  id,
  i18n
) {
  ns <- NS(id)
  card(
    id = ns("three_soil_types"),
    class = "mx-md-3 card-shadow mb-2",
    card_header(
      tags$h2(
        class = "card_title",
        i18n$translate("Shares of 3 Soil Types")
      )
    ),
    card_body(
      layout_columns(
        fill = FALSE,
        value_box(
          title = textOutput(ns("silt")),
          value = textOutput(ns("silt_val")),
          showcase = icon("layer-group"),
          theme = "success"
        ),
        value_box(
          title = textOutput(ns("clay")),
          value = textOutput(ns("clay_val")),
          showcase = icon("mug-saucer"),
          theme = "success"
        ),
        value_box(
          title = textOutput(ns("sand")),
          value = textOutput(ns("sand_val")),
          showcase = icon("hourglass-half"),
          theme = "success"
        )
      ),
    )
  )
}

#' @export
grassland_dynamics_three_soil_types_server <- function(
  id,
  soil_type_shares,
  tab_grassland_selected
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Define waiter ----
    msg <- waiter_text(message = tags$h3("Loading...", style = "color: #414f2f;"))
    w <- Waiter$new(
      id = ns("three_soil_types"),
      html = msg,
      color = "rgba(256,256,256,0.9)",
    )

    observeEvent(
      tab_grassland_selected(),
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      {
        w$show()
        soil_type_shares_reactive <- reactiveVal()
        soil_type_shares_reactive(soil_type_shares)

        output$silt <- renderText({
          names(soil_type_shares_reactive())[1]
        })
        output$clay <- renderText({
          names(soil_type_shares_reactive())[2]
        })
        output$sand <- renderText({
          names(soil_type_shares_reactive())[3]
        })

        output$silt_val <- renderText(soil_type_shares_reactive()[[1]])
        output$clay_val <- renderText(soil_type_shares_reactive()[[2]])
        output$sand_val <- renderText(soil_type_shares_reactive()[[3]])
        w$hide()
      }
    )
  })
}

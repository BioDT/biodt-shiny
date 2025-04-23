box::use(
  shiny[NS, moduleServer, icon, tags, textOutput, renderText, reactiveVal, observeEvent],
  bslib[card, card_header, card_body, value_box, layout_columns],
  waiter[Waiter],
)

box::use(
  app / logic / waiter[waiter_text],
)

#' @export
grassland_dynamics_soil_main_values_ui <- function(
  id,
  i18n
) {
  ns <- NS(id)
  card(
    id = ns("soil_main_values"),
    class = "mx-md-3 card-shadow mb-2",
    card_header(
      tags$h2(
        class = "card_title",
        i18n$translate("Main Values of Soil Data")
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
grassland_dynamics_soil_main_values_server <- function(
  id,
  main_values,
  tab_grassland_selected
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Define waiter ----
    msg <- waiter_text(message = tags$h3("Loading...", style = "color: #414f2f;"))
    w <- Waiter$new(
      id = ns("soil_main_values"),
      html = msg,
      color = "rgba(256,256,256,0.9)",
    )

    observeEvent(
      tab_grassland_selected(),
      ignoreNULL = TRUE,
      ignoreInit = TRUE,
      {
        w$show()
        main_values_reactive <- reactiveVal()
        main_values_reactive(main_values)

        output$silt <- renderText({
          names(main_values_reactive())[1]
        })
        output$clay <- renderText({
          names(main_values_reactive())[2]
        })
        output$sand <- renderText({
          names(main_values_reactive())[3]
        })

        output$silt_val <- renderText(main_values_reactive()[[1]])
        output$clay_val <- renderText(main_values_reactive()[[2]])
        output$sand_val <- renderText(main_values_reactive()[[3]])
        w$hide()
      }
    )
  })
}

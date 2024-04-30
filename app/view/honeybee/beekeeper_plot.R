box::use(
  shiny[moduleServer, NS, tagList, tags, selectInput, updateSelectInput, actionButton, reactiveVal, observeEvent],
  bslib[card, card_header],
  echarty[ecs.output, ecs.render, ec.init],
)

box::use(
  app/logic/waiter[waiter_text],
  app/logic/honeybee/honeybee_beekeeper_plot[honeybee_beekeeper_plot],
)

#' @export
beekeeper_plot_ui <- function(
    id,
    card_header = "Output plot",
    title = "output_plot",
    plot_width = "100%",
    plot_height = "500px"
    # custom_code = NULL
    ) {
  ns <- NS(id)
  tagList(
    card(
      class = "mx-md-3 card-shadow",
      id = ns("echarty_card"),
      title = title,
      full_screen = TRUE,
      card_header(
        tags$h5(card_header)
      ),
      selectInput(
        ns("experiment"),
        label = "Choose experiment:",
        choices = c(Example = "app/data/honeybee/output_example/Result_table_original.csv")
      ),
      actionButton(
        ns("update_plot"),
        label = "Update plot",
        style = "max-width: 200px"
      ),
      ecs.output(
        ns("echarty_plot"),
        width = plot_width,
        height = plot_height
      )
    )
  )
}

#' @export
beekeeper_plot_server <- function(
    id,
    beekeeper_selected,
    experiment_list,
    w) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    plot <- reactiveVal()

    observeEvent(beekeeper_selected(),
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        w$update(
          html = waiter_text(message = tags$h3("Preparing output plot...",
            style = "color: #414f2f;"
          ))
        )
        print("preparing plot")
        w$show()
        # Hardcoded for prototype
        honeybee_beekeeper_plot(
          "app/data/honeybee/output_example/Result_table_original.csv"
        ) |>
          plot()
        w$hide()
      }
    )

    observeEvent(experiment_list(),
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        print(experiment_list())
        print("updating list")
        shiny::updateSelectInput(
          inputId = "experiment",
          choices = experiment_list()
        )
      }
    )

    observeEvent(input$update_plot, {
      honeybee_beekeeper_plot(
        input$experiment
      ) |>
        plot()
    })

    output$echarty_plot <-
      ecs.render(
        plot()
      )
  })
}

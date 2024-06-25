box::use(
  shiny[moduleServer, NS, tagList, tags, selectInput, updateSelectInput, actionButton, reactiveVal, observeEvent, downloadButton, downloadHandler, req],
  bslib[card, card_header, layout_column_wrap],
  echarty[ecs.output, ecs.render, ec.init],
  waiter[Waiter],
  readr[write_csv],
  stringr[str_replace_all, str_remove],
)

box::use(
  app/logic/waiter[waiter_text],
  app/logic/honeybee/honeybee_beekeeper_plot[honeybee_beekeeper_plot, read_plot_data],
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
      class = "mx-md-3 card-shadow overflow-hidden",
      id = ns("echarty_card"),
      title = title,
      full_screen = TRUE,
      card_header(
        tags$h5(card_header)
      ),
      tags$div(
        class = "row",
        tags$div(
          class = "col-10",
          selectInput(
            ns("experiment"),
            label = "Choose experiment:",
            choices = c(Example = "app/data/honeybee/output_example/Result_table_original.csv")
          ),
          # style = "max-width: 200px"
        ),
        tags$div(
          class = "col-2 d-flex align-items-end flex-column",
          downloadButton(
            class = "",
            ns("download_data"),
            label = "Download plot data",
            # style = "max-width: 200px"
          )
        )
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
    experiment_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    plot <- reactiveVal()
    plot_data <- reactiveVal()

    msg <- waiter_text(
      message =
        tags$h3("Updating plot...",
          style = "color: #414f2f;"
        )
    )

    w <- Waiter$new(
      html = msg,
      color = "rgba(256,256,256,0.9)"
    )

    observeEvent(beekeeper_selected(),
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        req(
          beekeeper_selected(),
          is.null(plot_data())
        )
        print("preparing plot")
        w$show()
        # Hardcoded for prototype
        read_plot_data("app/data/honeybee/output_example/Result_table_original.csv") |>
          plot_data()

        honeybee_beekeeper_plot(
          input = plot_data()
        ) |>
          plot()
        w$hide()
      }
    )

    observeEvent(experiment_list(),
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        print("updating beekeeper plot list")
        new_list <- experiment_list()
        shiny::updateSelectInput(
          inputId = "experiment",
          choices = new_list,
          selected = new_list[length(new_list)]
        )
      }
    )

    # observeEvent(input$update_plot, {
    #   honeybee_beekeeper_plot(
    #     input$experiment
    #   ) |>
    #     plot()
    # })

    output$download_data <- downloadHandler(
      filename = function() { 
        paste0("honeybee_", names(experiment_list())[experiment_list() == input$experiment], "_", str_replace_all(str_replace_all(str_remove(Sys.time(), "\\.(.*)"), ":", "-"), " ", "_"),".csv")
      },
      content = function(file) {
        write_csv(plot_data(), file)
      },
    )

    observeEvent(
      input$experiment,
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      {
        # Start waiter ----
        w$show()

        read_plot_data(
          input$experiment
        ) |>
          plot_data()

        honeybee_beekeeper_plot(
          input = plot_data()
        ) |>
          plot()

        # Hide waiter ----
        w$hide()
      }
    )

    output$echarty_plot <-
      ecs.render(
        plot()
      )
  })
}

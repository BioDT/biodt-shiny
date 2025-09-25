box::use(
  shiny[
    moduleServer,
    NS,
    tagList,
    tags,
    actionButton,
    observeEvent,
    reactiveVal,
    bootstrapPage,
    req
  ],
  leaflet[leaflet, addTiles],
  bslib[layout_column_wrap],
  htmltools[css],
  waiter[Waiter],
  readr[read_csv],
  config,
)

box::use(
  app / view / honeybee / beekeeper_control[beekeeper_control_ui, beekeeper_control_server],
  app / view / honeybee / beekeeper_map[honeybee_map_ui, honeybee_map_server],
  app / view / honeybee / beekeeper_param[honeybee_param_ui, honeybee_param_server],
  app / view / honeybee / beekeeper_lookup[honeybee_lookup_ui, honeybee_lookup_server],
  app / view / honeybee / beekeeper_plot[beekeeper_plot_ui, beekeeper_plot_server],
  app /
    view /
    honeybee /
    beekeeper_runsimulation[
      beekeeper_runsimulation_ui,
      beekeeper_runsimulation_server
    ],
  app / logic / honeybee / honeybee_beekeeper_map[read_honeybee_tif, honeybee_leaflet_map],
  app / logic / waiter[waiter_text],
)

#' @export
honeybee_beekeeper_ui <- function(id, theme, i18n) {
  ns <- NS(id)
  tagList(
    tags$div(
      # UI module at the top of the given pDT,
      # describing to users step by step what to do, when interacting with the app
      beekeeper_control_ui(
        ns("beekeeper_control"),
        i18n
      ),
      layout_column_wrap(
        width = NULL,
        fill = FALSE,
        style = css(grid_template_columns = "3fr 1fr"),
        honeybee_map_ui(
          ns("beekeeper_map"),
          i18n
        ),
        honeybee_param_ui(
          ns("beekeeper_param"),
          theme,
          i18n
        )
      ),
      honeybee_lookup_ui(
        ns("beekeeper_lookup"),
        i18n
      ),
      beekeeper_runsimulation_ui(
        ns("beekeeper_runsimulation"),
        i18n
      ),
      beekeeper_plot_ui(
        ns("beekeeper_plot"),
        i18n
      )
    )
  )
}

#' @export
honeybee_beekeeper_server <- function(id, session_dir, beekeeper_selected, i18n) {
  moduleServer(id, function(input, output, session) {
    # Define waiter ----
    msg <- list(
      waiter_text(message = tags$h3(i18n$t("Loading data..."), style = "color: #414f2f;")),
      waiter_text(message = tags$h3(i18n$t("Computing Beehave simulation..."), style = "color: #414f2f;"))
    )

    w <- Waiter$new(
      html = msg[[1]],
      color = "rgba(256,256,256,0.9)"
    )

    # Variables ----
    map <- reactiveVal()
    leaflet_map <- reactiveVal()
    lookup_table <- reactiveVal()

    # Initialization ----
    observeEvent(
      beekeeper_selected(),
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      priority = 1000,
      {
        req(
          beekeeper_selected(),
          is.null(map()) ||
            is.null(lookup_table())
        )
        w$show()
        # Load map
        file.path(config$get("data_path"), "honeybee", "map.tif") |>
          read_honeybee_tif() |>
          map()

        map() |>
          honeybee_leaflet_map(
            add_control = TRUE,
            main_map_features = TRUE,
          ) |>
          leaflet_map()

        # Lookup table
        file.path(config$get("data_path"), "honeybee", "lookup_table.csv") |>
          read_csv(show_col_types = FALSE) |>
          lookup_table()

        w$hide()
      }
    )

    # Map ----
    coordinates <- honeybee_map_server(
      "beekeeper_map",
      leaflet_map = leaflet_map,
      experiment_list = experiment_list,
      map,
      i18n = i18n
    )

    # Parameters ----
    parameters <- honeybee_param_server("beekeeper_param")

    # Lookup table ----
    lookup <- honeybee_lookup_server("beekeeper_lookup", lookup_table = lookup_table, i18n)

    # Execution ----
    experiment_list <- beekeeper_runsimulation_server(
      "beekeeper_runsimulation",
      coordinates,
      lookup,
      parameters,
      map,
      session_dir,
      i18n
    )

    # Plot ----
    beekeeper_plot_server(
      "beekeeper_plot",
      beekeeper_selected,
      experiment_list,
      i18n
    )
  })
}

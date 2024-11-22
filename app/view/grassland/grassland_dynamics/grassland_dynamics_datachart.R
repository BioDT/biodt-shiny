box::use(
  shiny[NS, moduleServer, tags],
  bslib[card, card_header, card_body],
  echarty[ecs.output, ecs.render, ec.init],
  readr[read_delim]
)

box::use(
  app/logic/grassland/grassland_process_data_for_chart[filepaths_results, process_grassland_data]
)

#' @export
grassland_dynamics_datachart_ui <- function(
  id,
  i18n,
  plot_width = "100%",
  plot_height = "500px"
  ) {
  ns <- NS(id)
  card(
    id = ns("datachart"),
    class = "mx-md-3 card-shadow mb-2",
    full_screen = TRUE,
    card_header(
      tags$h2(
        class = "card_title",
        i18n$translate("PFTs Chart"))
    ),
    card_body(
      #column(12, ecs.output("pft_chart"))
      ecs.output(
        ns("pft_chart"),
        width = plot_width,
        height = plot_height
      )
    ),
  )
}

#' @export
grassland_dynamics_datachart_server <- function(id) { # nolint
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  # output$pft_chart <- ecs.render({
  #   p <- ec.init()
   
  # Echarty: making chart ----
chart1 <- ec.init()

time <- input_data <- read_delim(
  file = filepaths_results[1],
  #file = get_paths_of_all_files,
  skip = 0,
  trim_ws = TRUE,
  delim = "\t",
  escape_double = FALSE,
  col_names = TRUE,
  col_types = list(
    Date = "D",
    DayCount = "-",
    PFT = "-",
    Fraction = "-",
    NumberPlants = "-"
  )
)$Date |> unique()

# simulations <- purrr::map(filepaths_results,
#                           read_grass_simulations)

simulations <- NULL
for (i in 1:2){#length(filepaths_results)) {
  filepath <- filepaths_results[i]
  simulations <- simulations |>
    c(process_grassland_data(filepath, i))
}

# Prepare tooltip formatter
kl <- length(simulations) - 1 
formatter <- paste0('{a',kl,'} at time {b', kl,'} <br /> {c',kl,'}')


chart1$x$opts <-
  list(
    title = list(text = "Grassland simulation"),
    tooltip = list(trigger = "axis"#,
                   # formatter = formatter
                   ),
    legend = list(data = list("PFT0", "PFT1", "PFT2")),
    xAxis = list(
      type = "category",
      boundaryGap = TRUE,
      name = "Date",
      nameLocation = "middle",
      nameGap = 25,
      nameTextStyle = list(fontWeight = "bolder"),
      data = time
    ),
    yAxis = list(
      type = "value",
      boundaryGap = FALSE,
      name = "Fraction",
      nameLocation = "middle",
      nameGap = 40,
      nameTextStyle = list(fontWeight = "bolder"),
      min = 0,
      max = 100
    ),
    series = simulations
  )

  chart1

  })

}

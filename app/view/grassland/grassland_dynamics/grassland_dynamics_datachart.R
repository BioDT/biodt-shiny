box::use(
  shiny[NS, moduleServer, tags],
  bslib[card, card_header, card_body],
)

#' @export
grassland_dynamics_datachart_ui <- function(id, i18n) {
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
      column(12, ecs.output("pft_chart"))
    ),
  )
}

#' @export
grassland_dynamics_datachart_server <- function(id) { # nolint
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  output$pft_chart <- ecs.render({
    p <- ec.init()
   
  # Echarty: making chart ----
chart1 <- ec.init()

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

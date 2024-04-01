box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow]
)

#' @export
honeybee_info_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    class = "align-items-center justify-content-center",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center",
        tags$h2(
          class = "greeting display-4 font-weight-bold",
          "BEEHAVE"
        ),
        tags$p(
          class = "pt-3",
          "The pollinators prototype Digtial Twin is based on the mechanistic simulation model BEEHAVE (Becher et al. 2014, https://doi.org/10.1111/1365-2664.12222)."
        ),
        tags$br(),
        tags$p("Model descriptions and additional information can be found on https://beehave-model.net/."),
        tags$br(),
        tags$p("The pollinator prototype Digital Twin needs input on floral resources. As a demonstration example we use a land cover map provided by Preidl et al. (2020, https://doi.org/10.1594/PANGAEA.910837).")
      )
    ),
    column(
      width = 6,
      tags$div(
        class = "m-0 p-0 d-none d-lg-block",
        tags$img(
          src = "static/img/boba-jaglicic-mkk_9x42sbg-unsplash-min-scaled.jpg",
          style = "width: 1000px;"
        )
      )
    )
  )
}

#' @export
honeybee_info_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}

box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon],
)


#' @export
bee_contributors_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    class = "align-items-center justify-content-center m-0 p-0",
    style = "overflow-x: hidden",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      style = "height: 100vh;",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center mt-5",
        tags$h4("CONTRIBUTORS",
          style = "margin-bottom: 50px"
        ),
        tags$p("Jürgen Groeneveld and Volker Grimm, Department of Ecological Modelling, Helmholtz Centre for Environmental Research - UFZ, Permoserstr. 15, 7 04318 Leipzig, Germany"),
        tags$p("Tomas Martinovic and Ondrej Salamon, IT4Innovations, VSB – Technical University of Ostrava, 17. listopadu 2172/15, 708 00 Ostrava-Poruba, Czech Republic"),
        tags$p("Tuomas Rossi and Kata Sara-aho, CSC – IT Center for Science Ltd., P.O. Box 405, 02101 Espoo, Finland.")
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          src = "static/img/boba-jaglicic-mkk_9x42sbg-unsplash-min-scaled.jpg",
          style = "width: 50vw; height: 100vh; max-height: 1000px; object-fit: cover;"
        )
      )
    )
  )
}

#' @export
bee_contributors_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

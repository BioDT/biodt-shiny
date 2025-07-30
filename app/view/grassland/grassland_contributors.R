box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon],
)


#' @export
grassland_contributors_ui <- function(id, i18n) {
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
        tags$h2(
          i18n$translate("CONTRIBUTORS"),
          style = "greeting display-4 font-weight-bold"
        ),
        tags$p(
          "Thomas Banitz and Franziska Taubert, Department of Ecological Modelling, Helmholtz Centre for Environmental Research - UFZ, Permoserstr. 15, 04318 Leipzig, Germany"
        ),
        tags$p(
          "Tomas Martinovic and Ondrej Salamon, IT4Innovations, VSB – Technical University of Ostrava, 17. listopadu 2172/15, 708 00 Ostrava-Poruba, Czech Republic"
        ),
        tags$p("Tuomas Rossi, CSC – IT Center for Science Ltd., P.O. Box 405, 02101 Espoo, Finland.")
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          class = "info-picture",
          src = "static/img/grassland.jpg",
          alt = "",
        )
      )
    )
  )
}

#' @export
grassland_contributors_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

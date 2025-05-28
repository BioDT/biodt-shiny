box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon],
)


#' @export
cwr_contributors_ui <- function(id, i18n = NULL) {
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
          if (!is.null(i18n)) i18n$translate("CONTRIBUTORS") else "Contributors",
          style = "greeting display-4 font-weight-bold"
        ),
        tags$p(
          "Desalegn Chala, Dag Endresen and Erik Kusch – Natural History Museum, University of Oslo, Oslo, Norway "
        ),
        tags$p(
          "Tomas Martinovic - IT4Innovations, VSB - Technical University of Ostrava, Ostrava-Poruba, Czech Republic"
        ),
        tags$p("Tuomas Rossi - CSC – IT Center for Science Ltd., Espoo, Finland"),
        tags$p(
          "Claus Weiland, Jonas Grieb, Daniel Bauer - Senckenberg – Leibniz Institution for Biodiversity and Earth System Research, Science IT, Frankfurt am Main, Germany"
        ),
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          src = "static/img/ces/2048px-Cairngorms_National_Park_road_(Unsplash).jpg",
          style = "width: 50vw; height: 100vh; max-height: 1000px; object-fit: cover;"
        )
      )
    )
  )
}

#' @export
cwr_contributors_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

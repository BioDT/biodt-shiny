box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon],
)


#' @export
cwr_contributors_ui <- function(id, i18n) {
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
          src = "static/img/rye-5447847_1920.jpg",
          style = "width: 50vw; height: 100vh; max-height: 1000px; object-fit: cover;",
          alt = 'Image by <a href="https://pixabay.com/users/nickype-10327513/?utm_source=link-attribution&utm_medium=referral&utm_campaign=image&utm_content=5447847">Nicky ❤️🌿🐞🌿❤️</a> from <a href="https://pixabay.com//?utm_source=link-attribution&utm_medium=referral&utm_campaign=image&utm_content=5447847">Pixabay</a>'
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

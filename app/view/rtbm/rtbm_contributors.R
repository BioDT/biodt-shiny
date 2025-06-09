box::use(
  shiny[moduleServer, NS, column, tags, fluidRow, icon],
  htmltools[tagList, div, strong],
)


#' @export
rtbm_contributors_ui <- function(id, i18n = NULL) {
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
          "Allan T. Souza, INAR - Institute for Atmospheric and Earth System Research, University of Helsinki, Helsinki, Finland"
        ),
        tags$p("Anis U. Rahman, University of Jyväskylä, Jyväskylä, Finland"),
        tags$p("Ari Lehtiö, University of Jyväskylä, Jyväskylä, Finland"),
        tags$p("Aurélie Vancraeyenest, CSC - IT Center for Science, Espoo, Finland"),
        tags$p("Gleb Tikhonov, University of Helsinki, Helsinki, Finland and Aalto University, Espoo, Finland"),
        tags$p("Julian Lopez Gordillo, Naturalis Biodiversity Center, Netherlands"),
        tags$p("Jussi Talaskivi, University of Jyväskylä, Jyväskylä, Finland"),
        tags$p("Ossi Nokelainen, University of Jyväskylä, Jyväskylä, Finland"),
        tags$p("Otso Ovaskainen, University of Jyväskylä, Jyväskylä, Finland"),
        tags$p("Patrik Lauha, University of Helsinki, Helsinki, Finland"),
        tags$p("Zhenggang Gao, KTH – Royal Institute of Technology, Stockholm, Sweden")
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          class = "info-picture",
          src = "static/img/rtbm/bird.webp",
          alt = ""
        )
      )
    )
  )
}

#' @export
rtbm_contributors_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

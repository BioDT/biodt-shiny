box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon],
)


#' @export
forest_contributors_ui <- function(id, i18n) {
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
          "Bekir Afsar and Otso Ovaskainen, Department of Biological and Environmental Science, Faculty of Mathematics and Science, University of Jyväskylä, P.O. Box 35 (Survontie 9C), FI-40014, Jyväskylä, Finland"
        ),
        tags$p(
          "Kyle Eyvindson, Norwegian University of Life Sciences, Ås, Norway"
        ),
        tags$p(
          "Martijn Versluijs, Altenburg & Wymenga Ecological Consultants and Research, Suderwei 2, 9269 TZ Feanwâlden, Netherlands"
        ),
        tags$p(
          "Radek Halfar and Tomas Martinovic, IT4Innovations, VSB - Technical University of Ostrava, 708 00 Ostrava-Poruba, Czech Republic"
        )
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          class = "info-picture",
          src = "static/img/forest/trees-975091_1280.jpg",
          alt = "Forest. Image by Robert Balog from Pixabay",
        )
      )
    )
  )
}

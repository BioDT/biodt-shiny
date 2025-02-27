box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon],
)


#' @export
ces_contributors_ui <- function(id, i18n = NULL) {
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
        tags$p("Simon Rolph and Dylan Carbone, UK Centre for Ecology & Hydrology, Wallingford, United Kingdom"),
        tags$p("Jan Dick, Chris Andrews, Maddalena Tigli and Joe Marsh Rossney, UK Centre for Ecology & Hydrology, Edinburgh, United Kingdom"),
        tags$p("John Watkins and Will Bolton, UK Centre for Ecology & Hydrology, Lancaster, United Kingdom"),
        tags$p("Julian Lopez Gordillo, Naturalis Biodiversity Center, Leiden, Netherlands"),
        tags$p("Nick Oostervink, Netherlands Organisation for Applied Scientific Research, The Hague, Netherlands"),
        tags$p("Dirk Pleiter, KTH Royal Institute of Technology, Stockholm, Sweden"),
        tags$p("Christoph Wohner, Environment Agency Austria, Vienna, Austria"),
        tags$p("Tomas Martinovic, IT4Innovations, VSB – Technical University of Ostrava"),
        tags$p("Kata Sara-aho, CSC – IT Center for Science Ltd.")
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          src = "static/img/ces/2048px-Cairngorms_National_Park_road_(Unsplash).jpg",
          class = "info-picture",
          alt = "Cairngorms National Park"
        )
      )
    )
  )
}

#' @export
ces_contributors_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

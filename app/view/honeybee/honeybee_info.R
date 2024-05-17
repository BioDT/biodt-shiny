box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon],
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
          "HONEYBEE pDT"
        ),
        tags$p(
          class = "pt-3",
          "The HONEYBEE prototype Digital Twin (pDT) is based on the mechanistic simulation model BEEHAVE (Becher et al. 2014,", tags$a("https://doi.org/10.1111/1365-2664.12222", href = "https://doi.org/10.1111/1365-2664.12222"), ")."
        ),
        tags$p("Model descriptions of BEEHAVE and additional information can be found on", tags$a("https://beehave-model.net/", href = "https://beehave-model.net/"), "."),
        tags$p("The Honeybee prototype Digital Twin needs input on floral resources. As a demonstration example we use a land cover map provided by Preidl et al. (2020,", tags$a("https://doi.org/10.1594/PANGAEA.910837", href = "https://doi.org/10.1594/PANGAEA.910837"), ")."),
        tags$p("Weather data are requested from the Deutscher Wetterdienst (DWD) using an API provided by the R package rdwd (", tags$a("https://cran.r-project.org/web/packages/rdwd/index.html", href = "https://cran.r-project.org/web/packages/rdwd/index.html"), ")."),
        tags$p("Source code and scripts of the pDT can be found at ", tags$a(icon("github"), "https://github.com/BioDT", href = "https://github.com/BioDT"), "."),
        tags$h4("Contributors"),
        tags$p("Jürgen Groeneveld and Volker Grimm, Department of Ecological Modelling ,Helmholtz Centre for Environmental Research - UFZ, Permoserstr. 15, 7 04318 Leipzig, Germany"),
        tags$p("Tomas Martinovic and Ondrej Salamon, IT4Innovations, VSB – Technical University of Ostrava, 17. listopadu 2172/15, 708 00 Ostrava-Poruba, Czech Republic"),        
        tags$p("Tuomas Rossi and Kata Sara-aho, CSC – IT Center for Science Ltd., P.O. Box 405, 02101 Espoo, Finland.")
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

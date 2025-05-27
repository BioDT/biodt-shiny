box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon, actionButton, observeEvent],
  bslib[nav_select],
)

#' @export
forest_info_ui <- function(id, i18n) {
  ns <- NS(id)
  fluidRow(
    class = "fluid-row",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      style = "height: 100vh;",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center mt-5 mb-5",
        tags$h2(
          class = "greeting display-4 font-weight-bold",
          i18n$translate("Forest Biodiversity Prototype Digital Twin (pDT)")
        ),
        tags$p(
          tags$strong(i18n$translate("Purpose: ")),
          i18n$translate(
            "This pDT aims to investigate the impact of different forest management strategies and climate change scenarios on forests and biodiversity. The goal is to identify the most appropriate treatment option that improves biodiversity for a specific forest region under various climate scenarios."
          )
        ),
        tags$p(
          tags$a(i18n$translate("Replica of the Forest: ")),
          i18n$translate("Forest simulations conducted with LANDIS-II (Scheller and Mladenoff 2007, "),
          tags$a(
            "https://doi.org/10.1016/j.ecolmodel.2006.10.009",
            href = "https://doi.org/10.1016/j.ecolmodel.2006.10.009"
          ),
          i18n$translate("), which is a landscape model designed to simulate forest succession and disturbances.")
        ),
        tags$p(
          i18n$translate("LANDIS-II Home Page: "),
          tags$a("https://landis-ii.org/", href = "https://landis-ii.org/")
        ),
        tags$p(
          i18n$translate("Completely open-source with extensive documentation on GitHub: "),
          tags$a("https://github.com/LANDIS-II-Foundation", href = "https://github.com/LANDIS-II-Foundation")
        ),
        tags$p(
          tags$strong(i18n$translate("Replica of the Species Living in the Forest: ")),
          i18n$translate(
            "Biodiversity modeling was conducted using the joint species distribution model - HMSC (Ovaskainen et al., 2017, "
          ),
          tags$a("https://doi.org/10.1111/ele.12757", href = "https://doi.org/10.1111/ele.12757"),
          i18n$translate(")  for analyzing community ecological data.")
        ),
        tags$p(
          i18n$translate("HMSC Home Page: "),
          tags$a(
            "https://www.helsinki.fi/en/researchgroups/statistical-ecology/software/hmsc",
            href = "https://www.helsinki.fi/en/researchgroups/statistical-ecology/software/hmsc"
          ),
        ),
        tags$p(
          i18n$translate("HSMC R-package is available on CRAN "),
          tags$a(
            "https://cran.r-project.org/web/packages/Hmsc/index.html",
            href = "https://cran.r-project.org/web/packages/Hmsc/index.html"
          ),
          i18n$translate("), and the development version can be found on GitHub ("),
          tags$a("https://github.com/hmsc-r/HMSC", href = "https://github.com/hmsc-r/HMSC"),
          ")."
        ),
        tags$p(
          i18n$translate("Source code and scripts of the pDT can be found at "),
          tags$a(
            icon("github"),
            "https://github.com/BioDT/uc-forest-bird",
            href = "https://github.com/BioDT/uc-forest-bird"
          ),
          "."
        ),
        tags$div(
          class = "mt-5",
          actionButton(
            ns("start"),
            label = i18n$translate("Start prototyping"),
            width = "100%",
            class = "btn-secondary",
            style = "max-width: 200px"
          )
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


#' @export
forest_info_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    observeEvent(
      input$start,
      {
        nav_select(
          "tab",
          selected = "App",
          session = main_session
        )
      }
    )
  })
}

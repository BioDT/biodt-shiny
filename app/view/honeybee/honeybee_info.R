box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow, icon, actionButton, observeEvent],
  bslib[nav_select],
)

#' @export
honeybee_info_ui <- function(id, i18n) {
  ns <- NS(id)
  fluidRow(
    class = "align-items-center justify-content-center m-0 p-0",
    style ="overflow-x: hidden",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      style = "height: 100vh;",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center mt-5 mb-5",
        tags$h2(
          class = "greeting display-4 font-weight-bold",
          i18n$translate("Honeybee Prototype Digital Twin (DT)")
        ),
        tags$p(
          class = "pt-3",
          i18n$translate("The HONEYBEE prototype Digital Twin (pDT) is based on the mechanistic simulation model BEEHAVE."), # nolint: line_length_linter.
          tags$a(
            "(Becher et al. 2014, https://doi.org/10.1111/1365-2664.12222)",
            href = "https://doi.org/10.1111/1365-2664.12222"
          )
        ),
        tags$p(i18n$translate("Model descriptions of BEEHAVE and additional information can be found on"), tags$a("https://beehave-model.net/", href = "https://beehave-model.net/"), "."),
        tags$p(i18n$translate("The Honeybee prototype Digital Twin needs input on floral resources. As a demonstration example we use a land cover map provided by Preidl et al. (2020,"), tags$a("https://doi.org/10.1594/PANGAEA.910837", href = "https://doi.org/10.1594/PANGAEA.910837"), ")."),
        tags$p(i18n$translate("Weather data are requested from the Deutscher Wetterdienst (DWD) using an API provided by the R package rdwd ("), tags$a("https://cran.r-project.org/web/packages/rdwd/index.html", href = "https://cran.r-project.org/web/packages/rdwd/index.html"), ")."),
        tags$p(i18n$translate("Source code and scripts of the pDT can be found at "), tags$a(icon("github"), "https://github.com/BioDT", href = "https://github.com/BioDT"), "."),
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
          src = "static/img/boba-jaglicic-mkk_9x42sbg-unsplash-min-scaled.jpg",
          style = "width: 50vw; height: 100vh; max-height: 1000px; object-fit: cover;"
        )
      )
    )
  )
}




#' @export
honeybee_info_server <- function(id, main_session) {
  moduleServer(id, function(input, output, session) {
    observeEvent(
      input$start,
      {
        nav_select(
          "tab",
          selected = "Beekeeper",
          session = main_session
        )
      }
    )
  })
}

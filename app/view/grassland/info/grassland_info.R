box::use(
  shiny[NS, column, tags, fluidRow],
)

#' @export
grassland_info_ui <- function(id, i18n) {
  ns <- NS(id)
  fluidRow(
    id = ns("welcome"),
    class = "fluid-row",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      style = "height: 100vh;",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center mt-5 mb-5",
        tags$h2(
          class = "greeting display-4 font-weight-bold",
          i18n$translate("Grassland")
        ),
        tags$h6(i18n$translate("Background - scientific breakthroughs:")),
        tags$p(
          class = "pt-3",
          i18n$translate("Describe temporal biodiversity dynamics for different local grassland sites by a regionally transferable model (GRASSMIND), i.e. with a generic parameterization such that separate site-specific model calibration is avoided ('biodiversity' represented in the model by 4 plant functional groups: grasses, small herbs, tall herbs, legumes).")
        ),
        tags$br(),
        tags$p(i18n$translate("Project temporal changes in biodiversity dynamics (e.g. due to climate change or climate extremes) and provide scenario-based (management) options for mitigating undesired effects."))
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          class = "info-picture",
          src = "static/img/grassmind_long.jpg",
          alt = "",
        )
      )
    )
  )
}

# #' @export
# grassland_info_server <- function(id) {
#   moduleServer(id, function(input, output, session) {

#   })
# }

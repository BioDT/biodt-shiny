box::use(
  shiny[moduleServer, NS, tagList, div, column, tags, fluidRow],
)

#' @export
grassland_info_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    id = ns("welcome"),
    class = "align-items-center justify-content-center",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center",
        tags$h2(
          class = "greeting display-4 font-weight-bold",
          "GRASSLAND"
        ),
        tags$h6("Background - scientific breakthroughs:"),
        tags$p(
          class = "pt-3",
          "Describe temporal biodiversity dynamics for different local grassland sites
          by a regionally transferable model (GRASSMIND), i.e. with a generic parameterization 
          such that separate site-specific model calibration is avoided ('biodiversity' 
          represented in the model by 4 plant functional groups: grasses, small herbs, 
          tall herbs, legumes)."
        ),
        tags$br(),
        tags$p("Understand the dynamic interplay of environmental drivers, management regimes, 
        grassland dynamics & biodiversity in a process-based, quantitative and dynamic manner 
        (e.g. how much could the proportion of herbs change by less frequent cutting within one 
        year at humid sites?)"),
        tags$br(),
        tags$p("Project temporal changes in biodiversity dynamics (e.g. due to climate change or 
        climate extremes) and provide scenario-based (management) options for mitigating undesired 
        effects.")
      )
    ),
    column(
      width = 6,
      tags$div(
        class = "m-0 p-0 d-none d-lg-block",
        tags$img(
          src = "static/img/grassmind_long.jpg",
          style = "width: 1000px;"
        )
      )
    )
  )
}

#' @export
# grassland_info_server <- function(id) {
#   moduleServer(id, function(input, output, session) {

#   })
# }
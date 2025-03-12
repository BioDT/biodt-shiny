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
          class = "greeting display-6 fw-bold",
          i18n$translate("Grassland Biodiversity Dynamics")
        ),
        tags$h6(i18n$translate("Prototype Digital Twin (pDT) in EARLY ACCESS")),
        tags$p(
          class = "pt-3 text-danger fw-bold",
          i18n$translate(
            "This prototype Digital Twin is in early access and intended for research purposes only. Do not use for decision-making or operational purposes!"
          )
        ),
        tags$p(i18n$translate(
          "Explore the Digital Twin for grassland dynamics! Our Digital Twin is designed to help you understand the growth dynamics and improve the management of temperate grassland for sites across Europe. "
        )),
        tags$p(i18n$translate(
          "Once a specific site location has been selected, the pDT will simulate productivity (aboveground biomass and yield) and plant functional composition (grasses, forbs and legumes) over a selected time span in the past and, optionally, projected into the short-term future. Users may define and test the effects of different management scenarios. "
        ))
      )
    ),
    column(
      width = 6,
      style = "height: 100vh;",
      class = "d-none d-lg-block m-0 p-0",
      tags$div(
        tags$img(
          class = "info-picture",
          src = "static/img/grassland.jpg",
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

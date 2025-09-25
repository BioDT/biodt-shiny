box::use(
  shiny[
    moduleServer,
    NS,
    strong,
    em,
    h2,
    p,
    tagList,
    div,
    column,
    tags,
    fluidRow,
    icon,
    actionButton,
    observeEvent,
  ],
  bslib[nav_select],
)

#' @export
ias_info_ui <- function(id, i18n) {
  ns <- NS(id)
  fluidRow(
    class = "fluid-row",
    style = "overflow-x: hidden",
    column(
      width = 6,
      class = "col-sm-12 col-lg-6",
      style = "height: 100vh;",
      tags$div(
        class = "col-sm-10 offset-sm-1 text-center mt-5 mb-5",
        h2(i18n$t("Invasive Alien Species Digital Twin (DT)")),
        tags$p(
          class = "pt-3 fw-bold",
          i18n$translate(
            "This prototype Digital Twin is in early access and intended for research purposes only. Do not use for decision-making or operational purposes!"
          )
        ),
        p(
          i18n$translate("This dashboard provides projections of the distribution of invasive alien plant species (IAS) in Europe under current and future climate scenarios. Species distributions were modelled at the broad-habitat level using joint species distribution models. For each habitat type, the dashboard shows interactive gridded maps for the level of plant invasion (i.e., projected number of IAS) and species-specific habitat suitability. For more information on the IAS pDT, see Khan et al. 2024.")
        ),
        p(
          tags$strong(i18n$translate("pDT workflow & data server: ")),
          tags$a("https://github.com/BioDT/IASDT.R", href = "https://github.com/BioDT/IASDT.R", target = "_blank"),
          i18n$translate(" and "),
          tags$a(
            "https://github.com/BioDT/uc-ias-workflows",
            href = "https://github.com/BioDT/uc-ias-workflows",
            target = "_blank"
          )
        ),
        p(
          tags$strong(i18n$translate("Licence: ")),
          i18n$translate("The data may be used freely for scientific and policy purposes under the "),
          tags$a(
            "Creative Commons Licence CC BY 4.0",
            href = "https://creativecommons.org/licenses/by/4.0/",
            target = "_blank"
          )
        ),
        p(
          tags$strong(i18n$t("Contact: ")),
          i18n$translate("For more information, please contact: "),
          tags$a("Ahmed El-Gabbas", href = "mailto:elgabbas@outlook.com"),
          i18n$translate("(data & models);"),
          tags$a("Taimur Khan", href = "mailto:taimur.khan@ufz.de"),
          i18n$translate(" (pDT workflow & data server).")
        ),
        p(
          tags$strong(i18n$translate("Citations: ")),
          i18n$translate("soon")
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
          src = "static/img/ias/IAS2.png",
          alt = "",
        )
      )
    )
  )
}

#' @export
ias_info_server <- function(id, main_session, i18n) {
  moduleServer(id, function(input, output, session) {
    observeEvent(
      input$start,
      {
        nav_select(
          "tab",
          selected = "ias_app",
          session = main_session
        )
      }
    )
  })
}

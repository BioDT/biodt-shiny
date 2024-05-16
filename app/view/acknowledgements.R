box::use(
  shiny[NS, fillPage, div, p, moduleServer, tagList, tags, column, icon],
  bslib[card, card_header, card_body],
)

#' @export
mod_acknowledgements_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "column index-info",
    tags$div(
      class = "col",
      card(
        id = ns("acknowledgement"),
        class = "mt-2 mx-md-3 card-shadow",
        card_header(
          tags$div(
            class = "row d-flex justify-content-between align-items-center my-1",
            tags$div(
              class = "col-md-8 col-sm-12 me-auto",
              tags$h5("Acknowledgements"),
            ),
          )
        ),
        card_body(
          id = ns("acknowledgement"),
          tags$div(
            class = "row d-flex justify-content-between",
            tags$p("This project has received funding from the European Union's Horizon Europe research and innovation programme under grant agreement No 101057437 (BioDT project, https://doi.org/10.3030/101057437)."),
            tags$p("We acknowledge the EuroHPC Joint Undertaking and CSC – IT Center for Science, Finland for awarding this project access to the EuroHPC supercomputer LUMI, hosted by CSC – IT Center for Science and the LUMI consortium, through Development Access calls."),
            tags$p("Additionally, we wish to acknowledge CSC – IT Center for Science for Rahti and Pouta services."),
          )
        )
      ),
      card(
        id = ns("credits"),
        class = "mt-2 mx-md-3 card-shadow",
        card_header(
          tags$div(
            class = "row d-flex justify-content-between align-items-center my-1",
            tags$div(
              class = "col-md-8 col-sm-12 me-auto",
              tags$h5("Credits"),
            ),
          )
        ),
        card_body(
          id = ns("credits"),
          tags$div(
            class = "row d-flex justify-content-between acknowledgement-card",
            tags$p("Credits to the main development contributors"),
            tags$ul(
              tags$style("
            .acknowledgement-card ul li {
              margin-bottom: 10px;
              margin-left: 20px;
            }
          "),
              tags$li("Tomáš Martinovič, IT4Innovations, VSB - Technical University of Ostrava"),
              tags$li("Ondrej Salamon, IT4Innovations, VSB - Technical University of Ostrava"),
              tags$li("Allan Souza, University of Helsinki"),
              tags$li("Simon Rolph, UK Centre for Ecology & Hydrology"),
              tags$li("Kata Sara-aho, CSC - IT Center for Science"),
            ),
          ),
         tags$div(
           tags$p("You can report issues on ", tags$a(
             "Github", icon("github"),
             href = "https://github.com/BioDT/biodt-shiny/issues",
             target = "_blank"
           )),
           
         )
        )
      )
    )
  )
}

#' @export
mod_acknowledgements_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

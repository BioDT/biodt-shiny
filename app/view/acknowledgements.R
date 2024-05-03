box::use(
  shiny[NS, fillPage, div, p, moduleServer, tagList, tags],
  bslib[card, card_header, card_body],
)

#' @export
mod_acknowledgements_ui <- function(id) {
  ns <- NS(id)
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
        tags$p("Credits to the main development contributors"),
        tags$ul(
          tags$style("
            ul li {
              margin-bottom: 10px;
              margin-left: 20px;
            }
          "),
          tags$li("Tomas Martinovic, IT4I"),
          tags$li("Ondrej Salamon, IT4I"),
          tags$li("Allan Souza, University of Helsinki"),
          tags$li("Simon Rolph, UKCEH"),
          tags$li("Kata Sara-aho, CSC"),
        ),
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

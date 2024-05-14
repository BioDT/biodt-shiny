box::use(
  shiny[NS, fillPage, div, p, moduleServer, tags, h1, fluidRow],
  bslib[card, card_header, card_body],
)

box::use(
  app/view/honeybee/honeybee_main[honeybee_ui, honeybee_server],
)

#' @export
mod_info_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    tags$div(
      class = "row",
      tags$div(
        class = "col mx-auto col-lg-8",
        tags$h1(
          tags$span(
            class = "text-primary",
            "Prototype"
          ),
          "Digital Twins"
        ),

        tags$div(
          class = "row mt-5",
          tags$div(
            class = "col-md-6",
            tags$div(
              class = "landing-pdt-wrap",
              tags$div(
                class = "title",
                tags$h3("Species response to environmental change")
              ),
              tags$div(
                class = "description",
                tags$div(
                  class = "row",
                  tags$div(
                    class = "col-lg-6",
                    tags$div(
                      class = "img-box",
                      tags$img(
                        src = "static/img/usecase1.png",
                        width = 210,
                        height = 210,
                        loading = "lazy"
                      ),

                    )
                  ),
                  tags$div(
                    class = "col-lg-6",
                    tags$div(
                      class = "views-element-container",
                      tags$ul(
                        tags$li(
                          tags$a(
                            href = "#",
                            "Biodiversity Dynamics"
                          ),
                        ),
                        tags$li(
                          tags$a(
                            href = "#",
                            "Ecosystem Services"
                          ),
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          tags$div(
            class = "col-md-6",
            tags$div(
              class = "landing-pdt-wrap",
              tags$div(
                class = "title",
                tags$h3("Genetically detected biodiversity")
              ),
              tags$div(
                class = "description",
                tags$div(
                  class = "row",
                  tags$div(
                    class = "col-lg-6",
                    tags$div(
                      class = "img-box",
                      tags$img(
                        src = "static/img/usecase2.png",
                        width = 210,
                        height = 210,
                        loading = "lazy"
                      )
                    )
                  ),
                  tags$div(
                    class = "col-lg-6",
                    tags$div(
                      class = "views-element-container",
                      tags$ul(
                        tags$li(
                          tags$a(
                            href = "#",
                            "Crop wild relatives and genetic resources for food security"
                          )
                        ),
                        tags$li(
                          tags$a(
                            href = "#",
                            "DNA detected biodiversity in cryptic habitats"
                          )
                        ),
                      )
                    )
                  )
                )
              )
            )
          ),
        ),

        tags$div(
          class = "row",
          tags$div(
            class = "col-md-6",
            tags$div(
              class = "landing-pdt-wrap",
              tags$div(
                class = "title",
                tags$h3("Dynamics and threats from and for species of policy concern")
              ),
              tags$div(
                class = "description",
                tags$div(
                  class = "row",
                  tags$div(
                    class = "col-lg-6",
                    tags$div(
                      class = "img-box",
                      tags$img(
                        src = "static/img/usecase3.png",
                        width = 210,
                        height = 210,
                        loading = "lazy"
                      ),

                    )
                  ),
                  tags$div(
                    class = "col-lg-6",
                    tags$div(
                      class = "views-element-container",
                      tags$ul(
                        tags$li(
                          tags$a(
                            href = "#",
                            "Invasive Species"
                          ),
                        ),
                      )
                    )
                  )
                )
              )
            )
          ),
          tags$div(
            class = "col-md-6",
            tags$div(
              class = "landing-pdt-wrap",
              tags$div(
                class = "title",
                tags$h3("Species interactions with each other and with humans")
              ),
              tags$div(
                class = "description",
                tags$div(
                  class = "row",
                  tags$div(
                    class = "col-lg-6",
                    tags$div(
                      class = "img-box",
                      tags$img(
                        src = "static/img/usecase4.png",
                        width = 210,
                        height = 210,
                        loading = "lazy"
                      )
                    )
                  ),
                  tags$div(
                    class = "col-lg-6",
                    tags$div(
                      class = "views-element-container",
                      tags$ul(
                        tags$li(
                          tags$a(
                            href = "#",
                            "Pollinators"
                          )
                        ),
                        tags$li(
                          tags$a(
                            href = "#",
                            "Disease Outbreaks"
                          )
                        ),
                      )
                    )
                  )
                )
              )
            )
          ),
        ),


        tags$div(
          class = "info-text",
          tags$p(
            "The Biodiversity Digital Twin prototype will provide advanced models for simulation 
            and prediction capabilities, through practical use cases addressing critical issues 
            related to global biodiversity dynamics."
          ),
          tags$p(
            "BioDT exploits the LUMI Supercomputer and employs FAIR data combined with digital 
            infrastructure, predictive modelling and AI solutions, facilitating evidence-based 
            solutions for biodiversity protection and restoration."
          ),
          tags$p(
            "The project responds to key EU and international policy initiatives, including the EU 
            Biodiversity Strategy 2030, EU Green Deal, UN Sustainable Development Goals,
            Destination Earth."
          ),
        ),
      )
    ),

  )
}

#' @export
mod_info_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

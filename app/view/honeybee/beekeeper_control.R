box::use(
  shiny[moduleServer, NS, tags, actionButton, observeEvent],
  bslib[card, card_header],
  shinyjs[disabled, disable, enable],
  purrr[is_empty],
)

#' @export
beekeeper_control_ui <- function(id) {
  ns <- NS(id)
  card(
    id = ns("control"),
    class = "mt-2 mx-md-3 card-shadow",
    card_header(
      tags$div(
        class = "row d-flex justify-content-between align-items-center my-1",
        tags$div(
          class = "col-md-8 col-sm-12 me-auto",
          tags$h5("Control"),
        ),
        tags$div(
          class = "col-md-4 col-sm-12 d-flex flex-row justify-content-end",
          disabled(
            actionButton(
              ns("run_simulation"),
              label = "Run simulation",
              width = "100%",
              class = "btn-secondary",
              style = "max-width: 200px"
            )
          ),
          # shinyjs::disabled(
          #   shiny::actionButton(
          #     ns("load_resources"),
          #     label = "Update resources",
          #     width = "100%",
          #     class = "btn-secondary ms-1",
          #     style = "max-width: 200px"
          #   )
          # )
        )
      )
    )
  )
}

#' @export
beekeeper_control_server <- function(id,
                                     coordinates,
                                     lookup,
                                     parameters,
                                     w) {
  moduleServer(id, function(input, output, session) {
  
    # Prepare directory for results ----
    # Non-persistent data solution
    # Making the dir in the beekeep
    temp_dir <- tempdir()
    print(temp_dir)
    
    # Run workflow button ----
    observeEvent(
      coordinates(),
      {
        print(coordinates())
        if (!is_empty(coordinates()) &
            !is_empty(lookup()) &
            !is_empty(parameters())) {
          enable("run_simulation")
        } else {
          disable("run_simulation")
        }
      }
    )
    
    # Workflow execution ----
    observeEvent(
      input$run_simulation,
      {
        # Check data ----
        # Prepare folder structure ----

        
        
        # Prepare input data ----
        # Run workflow ----
        # Update output data ----
      }
    )
    
  })
}

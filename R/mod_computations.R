#' computations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList actionButton uiOutput
#' @importFrom bslib page_fluid
#' @importFrom DT DTOutput
mod_computations_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(bslib::page_fluid(
    shiny::actionButton(ns("update"),
                        label = "Update workflows"),
    shiny::uiOutput(ns("selectize_pdt")),
    DT::DTOutput(ns("workflows"))
  ))
}

#' computations Server Functions
#'
#' @noRd
#'
#' @importFrom shiny observeEvent renderUI selectizeInput tagList
#' @importFrom DT renderDataTable datatable
mod_computations_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    r_computations <- reactiveValues(df_workflows = NULL,
                                     beehave_workflow = "")
    
    # Set relevant workflow ids based on user permissions ----

      # Helper function to observe multiple events
      listen_workflow_inputs <- reactive({
        list(r$beehave_workflow_id,
             r$page_name)
      })
      
      
      shiny::observeEvent(listen_workflow_inputs(),
                          {
                            req(r$page_name == "Computations",
                                r$beehave_workflow_id)
                            
                            output$selectize_pdt <-
                              shiny::renderUI(shiny::tagList(
                                shiny::selectizeInput(
                                  ns("pdt"),
                                  label = "Choose pDT workflows to show:",
                                  choices = list(Beehave = r$beehave_workflow_id)
                                )
                              ))
                          })
      
      # Update workflow status ----
      # Helper function to observe multiple events
      listen_maps_input <- reactive({
        list(input$update,
             input$pdt,
             r$page_name)
      })
      
      shiny::observeEvent(listen_maps_input(),
                          {
                            req(r$lexis_token,
                                input$pdt,
                                r$page_name == "Computations")
                            
                            r_computations$df_workflows <-
                              r4lexis::get_workflow_states(r$lexis_token,
                                                           input$pdt) |>
                              dplyr::arrange(dplyr::desc(`Execution Date`))
                          })
      
      output$workflows <- DT::renderDataTable({
        req(r_computations$df_workflows)
        DT::datatable(
          r_computations$df_workflows,
          selection = "none",
          options = list(scrollX = TRUE),
          class = c("hover", "compact")
        ) |>
          DT::formatStyle(
            columns = "State",
            valueColumns = "State",
            target = "cell",
            color = DT::styleEqual(
              levels = c("running", "failed", "success"),
              values = c("blue", "red", "green")
            ),
            # backgroundColor = DT::styleEqual(c("running", "failed", "success"),
            #                                  c("rgba(256,256,0,0.3)", "rgba(256,0,0,0.3)", "rgba(0,256,0,0.3)"))
          ) |>
          DT::formatStyle(columns = "State",
                          target = "cell",
                          fontWeight = "bold")
      })
    })
}
## To be copied in the UI
# mod_computations_ui("computations_1")

## To be copied in the server
# mod_computations_server("computations_1")

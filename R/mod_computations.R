#' computations UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList actionButton
#' @importFrom bslib page_fluid 
#' @importFrom DT DTOutput
mod_computations_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fluid(
      shiny::actionButton(ns("update"),
                          label = "Update workflows"),
      shiny::selectizeInput(ns("pdt"),
                            label = "Choose pDT workflows to show:",
                            choices = list(Beehave = "biodt_beehave_karolina")),
      DT::DTOutput(ns("workflows"))
    )
  )
}
    
#' computations Server Functions
#'
#' @noRd 
#' 
#' @importFrom shiny observeEvent
#' @importFrom DT renderDataTable datatable
mod_computations_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    r_computations <- reactiveValues(
      df_workflows = NULL
    )
    
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
                   
                   r_computations$df_workflows <- r4lexis::get_workflow_states(
                     r$lexis_token,
                     input$pdt
                   )
                 })
    
    output$workflows <- DT::renderDataTable(
      {
        req(r_computations$df_workflows)
        datatable <- r_computations$df_workflows
      }
    )
  })
}
    
## To be copied in the UI
# mod_computations_ui("computations_1")
    
## To be copied in the server
# mod_computations_server("computations_1")

#' grassland UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib card card_header card_body
#' @importFrom DT DTOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs hidden
#' @importFrom ggplot2 ggplot geom_line aes
mod_grassland_ui <- function(id) {
  ns <- NS(id)
  tagList(bslib::page_fluid(
    class = "p-0",
    bslib::layout_sidebar(
      border = FALSE,
      # bslib::page_fluid(
      # theme = biodt_theme,
      sidebar = bslib::sidebar(
        shiny::h3("Location"),
        shiny::radioButtons(
          ns("input_type"),
          label = "Choose location type",
          choices = list("EDICD",
                         "Lat, Long"),
          selected = "EDICD"
        ),
        shiny::textInput(ns("edicd"),
                         "Input EDICD",
                         value = 5343),
        shinyjs::hidden(shiny::div(
          id = ns("latlon"),
          bslib::layout_column_wrap(
            width = 1 / 3,
            shiny::numericInput(ns("lat"),
                                label = "Latitude",
                                value = 0),
            shiny::numericInput(ns("lon"),
                                label = "Longitude",
                                value = 0)
          )
        )),
        shiny::h3("Workflow"),
        shinyjs::disabled(shiny::actionButton(ns("run_workflow"),
                                              label = "Run Workflow")),
      ),
      bslib::card(
        title = "input_map",
        full_screen = TRUE,
        card_title("Input Map"),
        card_body(leaflet::leafletOutput(ns("input_map_plot")))
      ),
      bslib::card(
        title = "output_gl",
        full_screen = TRUE,
        card_title("Model Output"),
        bslib::layout_column_wrap(
          width = 1 / 3,
          shiny::selectInput(ns("output_list"),
                             label = "Choose output dataset",
                             choices = NULL),
          shiny::selectInput(
            ns("output_files_list"),
            label = "Choose output files",
            choices = NULL,
            multiple = TRUE
          ),
          shinyjs::disabled(shiny::actionButton(ns("update_output"),
                                                label = "Show results"))
        ),
        plotOutput(ns("output_plot_gl"))
      ),
    )
  ))
}

#' grassland Server Functions
#'
#' @noRd
mod_grassland_server <- function(id,
                                 r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    shiny::observeEvent(input$input_type,
                        ignoreInit = TRUE,
                        {
                          print(input$input_type)
                          shinyjs::toggle(id = "latlon",
                                          condition = input$input_type == "Lat, Long")
                          shinyjs::toggle(id = "edicd",
                                          condition = input$input_type == "EDICD")
                        })
    
  })
}

## To be copied in the UI
# mod_grassland_ui("grassland_1")

## To be copied in the server
# mod_grassland_server("grassland_1")

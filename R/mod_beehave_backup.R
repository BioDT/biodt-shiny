#' beehave UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import gridlayout
#' @importFrom DT DTOutput
mod_beehave_ui2 <- function(id) {
  ns <- NS(id)
  tagList(
    grid_page(
      layout = c(
        "input_map locations",
        "lookup_table parameters_table",
        "output_bees_plot output_bees_plot",
        "output_honey_plot output_honey_plot",
        "output_map output_map"
      ),
      row_sizes = c("1fr",
                    "1fr",
                    "1fr",
                    "1fr",
                    "1fr"),
      col_sizes = c("2fr",
                    "1fr"),
      gap_size = "1rem",
      grid_card(
        area = "input_map",
        full_screen = TRUE,
        card_title("Input Map"),
        card_body(plotOutput(ns("input_map_plot")))
      ),
      grid_card(
        area = "locations",
        full_screen = TRUE,
        card_title("Locations"),
        card_body(),
        min_height = "500px"
      ),
      accordion(
        "outputs_accordion",
        grid_card(
          area = "lookup_table",
          full_screen = TRUE,
          card_title("Lookup Table"),
          card_body(DTOutput(ns("lookup_table"))),
          min_height = "500px"
        ),
        grid_card(
          area = "parameters_table",
          full_screen = TRUE,
          card_title("Parameters Table"),
          card_body(DTOutput(ns(
            "parameters_table"
          ))),
          min_height = "500px"
        ),
        grid_card(
          area = "output_bees_plot",
          full_screen = TRUE,
          card_title("Output Bees Plot"),
          card_body(plotOutput(ns(
            "output_bees_plot"
          ))),
          min_height = "400px"
        ),
        grid_card(
          area = "output_honey_plot",
          full_screen = TRUE,
          card_title("Output Honey Plot"),
          card_body(plotOutput(ns(
            "output_honey_plot"
          ))),
          min_height = "400px"
        ),
        grid_card(
          area = "output_map",
          full_screen = TRUE,
          card_title("Output Map"),
          card_body(plotOutput(ns(
            "output_map_plot"
          ))),
          min_height = "500px"
        )
      )
    )
  )
}

#' beehave Server Functions
#'
#' @importFrom DT renderDT
#' @importFrom shinipsum random_DT random_ggplot
#'
#' @noRd
mod_beehave_server2 <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    
    output$input_map_plot <- renderPlot({
      random_ggplot(type = "hex")
    })
    output$lookup_table <- DT::renderDT({
      random_DT(50, 4)
    })
    output$parameters_table <- DT::renderDT({
      random_DT(50, 3)
    })
    output$output_bees_plot <- renderPlot({
      random_ggplot(type = "line")
    })
    output$output_honey_plot <- renderPlot({
      random_ggplot(type = "line")
    })
    output$output_map_plot <- renderPlot({
      random_ggplot(type = "hex")
    })
    
  })
}

## To be copied in the UI
# mod_beehave_ui("beehave_1")

## To be copied in the server
# mod_beehave_server("beehave_1")

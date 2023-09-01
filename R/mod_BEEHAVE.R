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
mod_beehave_ui <- function(id) {
  ns <- NS(id)
  tagList(
    grid_page(
      layout =
        c(
          "input_map    locations",
          "lookup_table parameters_table",
          "output_bees  output_bees",
          "output_honey output_honey",
          "output_map   output_map"
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
        card_body(
          shiny::selectInput(ns("input_map_list"),
                             label = "Choose input map",
                             choices = NULL),
          plotOutput(ns("input_map_plot")))
      ),
      grid_card(
        area = "locations",
        full_screen = TRUE,
        min_height = "500px",
        card_title("Locations"),
        card_body()
      ),
      grid_card(
        area = "lookup_table",
        full_screen = TRUE,
        min_height = "500px",
        card_title("Lookup Table"),
        card_body(DTOutput(ns("lookup_table")))
      ),
      grid_card(
        area = "parameters_table",
        full_screen = TRUE,
        min_height = "500px",
        card_title("Parameters Table"),
        card_body(DTOutput(ns(
          "parameters_table"
        )))
      ),
      grid_card(
        area = "output_bees",
        full_screen = TRUE,
        min_height = "400px",
        card_title("Output Bees Plot"),
        card_body(plotOutput(ns(
          "output_bees_plot"
        )))
      ),
      grid_card(
        area = "output_honey",
        full_screen = TRUE,
        min_height = "400px",
        card_title("Output Honey Plot"),
        card_body(plotOutput(ns(
          "output_honey_plot"
        )))
      ),
      grid_card(
        area = "output_map",
        full_screen = TRUE,
        min_height = "500px",
        card_title("Output Map"),
        card_body(plotOutput(ns(
          "output_map_plot"
        )))
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
mod_beehave_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    r_beehave <- reactiveValues(
      input_map_list = NULL,
      map_files = NULL
    )
    
    observeEvent(r$lexis_token,
                 {
                   req(r$lexis_token,
                       r$lexis_dataset_list)
                   
                   ds_id <- purrr::map_chr(r$lexis_dataset_list, function(x){purrr::pluck(x, "location", "internalID")})
                   ds_titles <- purrr::map(r$lexis_dataset_list, function(x){purrr::pluck(x, "metadata", "title")}) |> unlist()
                   
                   ind_titles_true <- purrr::map_lgl(ds_titles, ~length(.x)>0)
                   ind_maps <- ds_titles == "Beehave Input Maps"
                   
                   r_beehave$map_files <- r4lexis::get_dataset_file_list(
                     r$lexis_token,
                     internalID = ds_id[ind_titles_true][ind_maps],
                     project = "biodt_development"
                   )
                   
                   beehave_map_list <- r_beehave$map_files$contents |>
                     purrr::map_chr(purrr::pluck("name")) 
                   
                   r_beehave$input_map_list <-  beehave_map_list[stringr::str_detect(beehave_map_list,
                                                                             ".tif$")]
                   
                   
                   updateSelectInput(
                     session = session,
                     inputId = "input_map_list",
                     selected = beehave_map_list[1],
                     choices = beehave_map_list
                   )
                   
                   
                   ind_lookup_table <- ds_titles == "Beehave Input Lookup"
                 })
    
    
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

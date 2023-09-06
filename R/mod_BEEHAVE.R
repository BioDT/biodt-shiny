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
#' @importFrom leaflet leafletOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs hidden
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
          shiny::selectInput(ns("map_list"),
                             label = "Choose input map",
                             choices = NULL),
          shinyjs::hidden(shiny::actionButton(ns("load_maps"),
                                              label = "Load maps")),
          shinycssloaders::withSpinner(leaflet::leafletOutput(ns("input_map_plot")))
        )
      ),
      grid_card(
        area = "locations",
        full_screen = TRUE,
        min_height = "500px",
        card_title("Locations"),
        card_body(
          shiny::numericInput(ns("latitude"),
                              label = "Latitude",
                              value = 0,
                              min = -90,
                              max = 90),
          shiny::numericInput(ns("longitude"),
                              label = "longitude",
                              value = 0,
                              min = -180,
                              max = 180),
          shiny::fileInput(ns("upload_dbf"),
                           label = "Upload dbf locations file",
                           accept = ".dbf")
        )
      ),
      grid_card(
        area = "lookup_table",
        full_screen = TRUE,
        min_height = "500px",
        card_title("Lookup Table"),
        card_body(
          shiny::selectInput(ns("lookup_list"),
                             label = "Choose input lookup table",
                             choices = NULL),
          shinyjs::hidden(shiny::actionButton(ns("load_lookup"),
                                              label = "Load lookup table")),
          DTOutput(ns("lookup_table")))
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
#' @importFrom shinyjs showElement hideElement
#' @importFrom r4lexis download_file_from_dataset extract_dataset get_dataset_file_list
#' @importFrom terra rast plet
#' @importFrom purrr map_chr pluck
#' @importFrom leaflet renderLeaflet
#' @importFrom golem print_dev
#'
#' @noRd
mod_beehave_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Define reactive variables ----
    r_beehave <- reactiveValues(
      input_map_list = NULL,
      input_rast_map = NULL,
      input_leaflet_map = NULL,
      map_ds = NULL,
      map_files = NULL,
      maps_loaded = FALSE,
      map_output = NULL,
      lookup_ds = NULL,
      lookup_files = NULL,
      lookup_loaded = FALSE,
      lookup_table = NULL,
      input_lookup_list = NULL,
    )
    
    # Define beehave variables ----
    temp_dir <- file.path(tempdir(), "beehave")
    if (!dir.exists(temp_dir)) {
      dir.create(temp_dir)
    }
    beehave_maps_dir <- file.path(temp_dir, "beehave_maps")
    if (!dir.exists(beehave_maps_dir)) {
      dir.create(beehave_maps_dir)
    }
    beehave_lookup_dir <- file.path(temp_dir, "beehave_lookup")
    if (!dir.exists(beehave_lookup_dir)) {
      dir.create(beehave_lookup_dir)
    }
    beehave_locations_dir <-
      file.path(temp_dir, "beehave_locations")
    if (!dir.exists(beehave_locations_dir)) {
      dir.create(beehave_locations_dir)
    }
    beehave_parameter_dir <-
      file.path(temp_dir, "beehave_parameter")
    if (!dir.exists(beehave_parameter_dir)) {
      dir.create(beehave_parameter_dir)
    }
    
    # Load maps and lookup table list ----
    observeEvent(r$lexis_token,
                 {
                   req(r$lexis_token,
                       r$lexis_dataset_list)
                   
                   
                   r_beehave$map_ds <-
                     r4lexis::extract_dataset(r$lexis_dataset_list,
                                              "Beehave Input Maps",
                                              "metadata",
                                              "title")
                   
                   r_beehave$map_files <-
                     r4lexis::get_dataset_file_list(
                       r$lexis_token,
                       internalID = r_beehave$map_ds$location$internalID,
                       project = "biodt_development"
                     )
                   
                   beehave_map_list <-
                     r_beehave$map_files$contents |>
                     purrr::map_chr(purrr::pluck("name"))
                   
                   r_beehave$input_map_list <-
                     beehave_map_list[stringr::str_detect(beehave_map_list,
                                                          ".tif$")]
                   
                   updateSelectInput(
                     session = session,
                     inputId = "map_list",
                     selected = r_beehave$input_map_list[1],
                     choices = r_beehave$input_map_list
                   )
                   
                   golem::print_dev(input$map_list)
                   
                   # Show download maps button if they are not downloaded
                   if (length(r_beehave$input_map_list) > 0 &
                       r_beehave$maps_loaded == FALSE) {
                     golem::print_dev("Showing map loading button.")
                     shinyjs::showElement("load_maps")
                   }
                   
                   r_beehave$lookup_ds <-
                     r4lexis::extract_dataset(r$lexis_dataset_list,
                                              "Beehave Input Lookup",
                                              "metadata",
                                              "title")
                   
                   r_beehave$lookup_files <-
                     r4lexis::get_dataset_file_list(
                       r$lexis_token,
                       internalID = r_beehave$lookup_ds$location$internalID,
                       project = "biodt_development"
                     )
                   
                   beehave_lookup_list <-
                     r_beehave$lookup_files$contents |>
                     purrr::map_chr(purrr::pluck("name"))
                   
                   r_beehave$input_lookup_list <-
                     beehave_lookup_list[stringr::str_detect(beehave_lookup_list,
                                                          ".csv$")]
                   
                   updateSelectInput(
                     session = session,
                     inputId = "lookup_list",
                     selected = r_beehave$input_lookup_list[1],
                     choices = r_beehave$input_lookup_list
                   )
                   
                   # Show download maps button if they are not downloaded
                   if (length(r_beehave$input_lookup_list) > 0 &
                       r_beehave$lookup_loaded == FALSE) {
                     golem::print_dev("Showing lookup loading button.")
                     shinyjs::showElement("load_lookup")
                   }
                   
                 })
    
    # Download maps from Lexis DDI ----
    
    observeEvent(input$load_maps,
                 
                 {
                   req(r$lexis_token)
                   
                   non_dev <- TRUE
                   if (length(list.files(beehave_maps_dir)) > 0 &
                       golem::app_dev()) {
                     r_beehave$maps_loaded <- TRUE
                     shinyjs::hideElement("load_maps")
                     non_dev <- NULL
                   }
                   
                   req(non_dev,
                       cancelOutput = TRUE)
                   golem::print_dev("Input maps button pressed.")
                   r4lexis::download_file_from_dataset(
                     r$lexis_token,
                     r_beehave$map_ds,
                     destination_path = beehave_maps_dir,
                     extract = TRUE
                   )
                   
                   if (length(list.files(beehave_maps_dir)) > 0) {
                     golem::print_dev("Input maps downloaded.")
                     r_beehave$maps_loaded <- TRUE
                     shinyjs::hideElement("load_maps")
                   } else {
                     golem::print_dev("Input maps NOT downloaded.")
                   }
                   
                 })
    
    # Helper function to observe multiple events
    listen_maps_input <- reactive({
      list(input$map_list,
           r_beehave$maps_loaded)
    })
    observeEvent(listen_maps_input(),
                 {
                   req(r_beehave$maps_loaded,
                       input$map_list)
                   golem::print_dev("Loading input map raster and making leaflet visualization.")
                   r_beehave$input_rast_map <-
                     terra::rast(file.path(beehave_maps_dir, input$map_list))
                   r_beehave$input_leaflet_map <-
                     terra::plet(r_beehave$input_rast_map,
                                 tiles = "Streets",
                                 alpha = 0.4)
                 })
    
    # Download lookup tables from Lexis DDI ----
    
    observeEvent(input$load_lookup,
                 
                 {
                   req(r$lexis_token)
                   
                   non_dev <- TRUE
                   if (length(list.files(beehave_lookup_dir)) > 0 &
                       golem::app_dev()) {
                     r_beehave$lookup_loaded <- TRUE
                     shinyjs::hideElement("load_lookup")
                     non_dev <- NULL
                   }
                   
                   req(non_dev,
                       cancelOutput = TRUE)
                   golem::print_dev("Input lookup button pressed.")
                   r4lexis::download_file_from_dataset(
                     r$lexis_token,
                     r_beehave$lookup_ds,
                     destination_path = beehave_lookup_dir,
                     extract = TRUE
                   )
                   
                   if (length(list.files(beehave_lookup_dir)) > 0) {
                     golem::print_dev("Input lookup tables downloaded.")
                     r_beehave$lookup_loaded <- TRUE
                     shinyjs::hideElement("load_lookup")
                   } else {
                     golem::print_dev("Input lookup tables NOT downloaded.")
                   }
                   
                 })
    
    # Helper function to observe multiple events
    listen_lookup_input <- reactive({
      list(input$lookup_list,
           r_beehave$lookup_loaded)
    })
    observeEvent(listen_lookup_input(),
                 {
                   req(r_beehave$lookup_loaded,
                       input$lookup_list)
                   golem::print_dev("Loading lookup tables.")
                   r_beehave$lookup_table <-
                     readr::read_csv(file.path(beehave_lookup_dir, input$lookup_list),
                                     show_col_types = FALSE)
                 })
    
    # Render outputs ----
    
    output$input_map_plot <-
      leaflet::renderLeaflet(r_beehave$input_leaflet_map)
    output$lookup_table <- DT::renderDT({
      DT::datatable(r_beehave$lookup_table)
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

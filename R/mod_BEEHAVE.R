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
          "load_button",
          "input_map",
          "lookup_table",
          "parameters_table",
          "run_workflow",
          "output_bees",
          "output_honey",
          "output_map"
        ),
      row_sizes = c(
        "80px",
        "700px",
        "650px",
        "650px",
        "80px",
        "500px",
        "500px",
        "500px"
      ),
      col_sizes = c("1fr"),
      gap_size = "1rem",
      grid_card(
        area = "load_button",
        full_screen = TRUE,
        shinyjs::hidden(shiny::actionButton(ns("load_resources"),
                                            label = "Load beehave resources"))
      ),
      grid_card(
        area = "input_map",
        full_screen = TRUE,
        card_title("Input Map"),
        card_body(
          shiny::selectInput(ns("map_list"),
                             label = "Choose input map",
                             choices = NULL),
          
          shiny::fileInput(ns("upload_dbf"),
                           label = "Upload dbf locations file",
                           accept = ".dbf"),
          # shinyjs::hidden(shiny::actionButton(ns("load_maps"),
          #                                     label = "Load maps")),
          shinycssloaders::withSpinner(leaflet::leafletOutput(ns("input_map_plot")))
        )
      ),
      grid_card(
        area = "lookup_table",
        full_screen = TRUE,
        card_title("Lookup Table"),
        card_body(
          shiny::selectInput(ns("lookup_list"),
                             label = "Choose input lookup table",
                             choices = NULL),
          # shinyjs::hidden(shiny::actionButton(ns("load_lookup"),
          #                                     label = "Load lookup table")),
          DTOutput(ns("lookup_table"))
        )
      ),
      grid_card(
        area = "parameters_table",
        full_screen = TRUE,
        min_height = "500px",
        card_title("Parameters Table"),
        card_body(
          DTOutput(ns("parameters_table")),
          shiny::selectInput(
            ns("parameter_name"),
            label = "Choose parameter",
            choices = c(
              'ABANDON_POLLEN_PATCH_PROB_PER_S',
              'ENERGY_HONEY_per_g',
              'MAX_BROODCELLS',
              'MAX_EGG_LAYING',
              'QueenAgeing',
              'MAX_km_PER_DAY',
              'AllowReinfestation',
              'N_INITIAL_MITES_INFECTED',
              'POLLEN_STORE_INIT',
              'FORAGER_NURSING_CONTRIBUTION',
              'SQUADRON_SIZE',
              'CRITICAL_COLONY_SIZE_WINTER',
              'MORTALITY_EGGS',
              'HATCHING_AGE',
              'MORTALITY_DRONE_EGGS',
              'DRONE_HATCHING_AGE',
              'EMERGING_AGE',
              'MAX_BROOD_NURSE_RATIO',
              'DRONE_EGGS_PROPORTION',
              'SEASON_START',
              'SEASON_STOP',
              'DRONE_PUPATION_AGE',
              'DRONE_EMERGING_AGE',
              'PRE_SWARMING_PERIOD',
              'N_INITIAL_MITES_HEALTHY',
              'N_INITIAL_MITES_INFECTED',
              'POST_SWARMING_PERIOD',
              'AFF_BASE',
              'MAX_AFF',
              'CROPVOLUME',
              'DRONE_EGGLAYING_START',
              'DRONE_EGGLAYING_STOP',
              'MORTALITY_LARVAE',
              'PUPATION_AGE',
              'MORTALITY_DRONE_LARVAE',
              'MORTALITY_PUPAE',
              'MORTALITY_DRONE_PUPAE',
              'MIN_AFF',
              'MAX_AFF',
              'MORTALITY_INHIVE',
              'MORTALITY_INHIVE_INFECTED_AS_PUPA',
              'MORTALITY_DRONES',
              'DRONE_LIFESPAN',
              'MAX_TOTAL_KM',
              'STEPWIDTH',
              'STEPWIDTHdrones',
              'FLIGHT_VELOCITY',
              'MAX_PROPORTION_POLLEN_FORAGERS',
              'LIFESPAN',
              'MIN_IDEAL_POLLEN_STORE',
              'PROTEIN_STORE_NURSES_d',
              'TIME_NECTAR_GATHERING',
              'TIME_POLLEN_GATHERING',
              'TIME_UNLOADING',
              'TIME_UNLOADING_POLLEN',
              'N_INITIAL_MITES_HEALTHY',
              'N_INITIAL_MITES_INFECTED',
              'INPUT_FILE',
              'WEIGHT_WORKER_g',
              'FIND_DANCED_PATCH_PROB',
              'FLIGHTCOSTS_PER_m',
              'FORAGING_STOP_PROB',
              'MAX_DANCE_CIRCUITS',
              'POLLEN_DANCE_FOLLOWERS',
              'POLLENLOAD',
              'MORTALITY_FOR_PER_SEC',
              'ENERGY_SUCROSE',
              'N_GENERIC_PLOTS',
              'MITE_FALL_DRONECELL',
              'MITE_FALL_WORKERCELL',
              'MITE_MORTALITY_BROODPERIOD',
              'MITE_MORTALITY_WINTER',
              'DISTANCE_G',
              'DISTANCE_R',
              'QUANTITY_G_l',
              'POLLEN_G_kg',
              'SHIFT_R',
              'SHIFT_G',
              'QUANTITY_R_l',
              'QUANTITY_G_l',
              'POLLEN_R_kg',
              'POLLEN_G_kg',
              'CONC_G',
              'CONC_R',
              'DETECT_PROB_G',
              'DETECT_PROB_R',
              'DANCE_SLOPE',
              'DANCE_INTERCEPT'
            )
          ),
          shiny::actionButton(ns("add_parameter"),
                              label = "Add parameter")
        )
      ),
      grid_card(
        area = "run_workflow",
        full_screen = TRUE,
        shinyjs::disabled(shiny::actionButton(ns("run_workflow"),
                                              label = "Run Workflow"))
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
      input_parameters = data.frame(
        Parameter = c("N_INITIAL_BEES", "MAX_HONEY_STORE_kg"),
        Value = c(3000, 50),
        `Default Value` = c(3000, 50)
      )
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
    
    constant_defaults <- init_const()
    
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
                   
                   # Show download resources button if they are not downloaded
                   if ((length(r_beehave$input_map_list) > 0 &
                        r_beehave$maps_loaded == FALSE) |
                       (length(r_beehave$input_lookup_list) > 0 &
                        r_beehave$lookup_loaded == FALSE)) {
                     golem::print_dev("Showing resources loading button.")
                     shinyjs::showElement("load_resources")
                   }
                   
                   # if (length(r_beehave$input_map_list) > 0 &
                   #     r_beehave$maps_loaded == FALSE) {
                   #   golem::print_dev("Showing map loading button.")
                   #   shinyjs::showElement("load_maps")
                   # }
                   
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
                   # if (length(r_beehave$input_lookup_list) > 0 &
                   #     r_beehave$lookup_loaded == FALSE) {
                   #   golem::print_dev("Showing lookup loading button.")
                   #   shinyjs::showElement("load_lookup")
                   # }
                   
                 })
    
    # Download maps from Lexis DDI ----
    
    observeEvent(#input$load_maps,
      input$load_resources,
      
      {
        req(r$lexis_token)
        
        # Map part ----
        non_dev <- TRUE
        if (length(list.files(beehave_maps_dir)) > 0 &
            golem::app_dev()) {
          r_beehave$maps_loaded <- TRUE
          # shinyjs::hideElement("load_maps")
          shinyjs::hideElement("load_resources")
          non_dev <- NULL
        }
        
        if (length(list.files(beehave_lookup_dir)) > 0 &
            golem::app_dev()) {
          r_beehave$lookup_loaded <- TRUE
          # shinyjs::hideElement("load_lookup")
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
          # shinyjs::hideElement("load_maps")
        } else {
          golem::print_dev("Input maps NOT downloaded.")
        }
        
        # Lookup part ----
        
        # non_dev <- TRUE
        # if (length(list.files(beehave_lookup_dir)) > 0 &
        #     golem::app_dev()) {
        #   r_beehave$lookup_loaded <- TRUE
        #   # shinyjs::hideElement("load_lookup")
        #   non_dev <- NULL
        # }
        
        r4lexis::download_file_from_dataset(
          r$lexis_token,
          r_beehave$lookup_ds,
          destination_path = beehave_lookup_dir,
          extract = TRUE
        )
        
        if (length(list.files(beehave_lookup_dir)) > 0) {
          golem::print_dev("Input lookup tables downloaded.")
          r_beehave$lookup_loaded <- TRUE
          # shinyjs::hideElement("load_lookup")
          shinyjs::hideElement("load_resources")
        } else {
          golem::print_dev("Input lookup tables NOT downloaded.")
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
    
    # observeEvent(input$load_lookup,
    #
    #              {
    #                req(r$lexis_token)
    #
    #                non_dev <- TRUE
    #                if (length(list.files(beehave_lookup_dir)) > 0 &
    #                    golem::app_dev()) {
    #                  r_beehave$lookup_loaded <- TRUE
    #                  shinyjs::hideElement("load_lookup")
    #                  non_dev <- NULL
    #                }
    #
    #                req(non_dev,
    #                    cancelOutput = TRUE)
    #                golem::print_dev("Input lookup button pressed.")
    #                r4lexis::download_file_from_dataset(
    #                  r$lexis_token,
    #                  r_beehave$lookup_ds,
    #                  destination_path = beehave_lookup_dir,
    #                  extract = TRUE
    #                )
    #
    #                if (length(list.files(beehave_lookup_dir)) > 0) {
    #                  golem::print_dev("Input lookup tables downloaded.")
    #                  r_beehave$lookup_loaded <- TRUE
    #                  shinyjs::hideElement("load_lookup")
    #                } else {
    #                  golem::print_dev("Input lookup tables NOT downloaded.")
    #                }
    #
    #              })
    
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
    
    # Lookup table edit logic ----
    
    observeEvent(input$lookup_table_cell_edit, {
      r_beehave$lookup_table[input$lookup_table_cell_edit$row,
                             input$lookup_table_cell_edit$col] <-
        input$lookup_table_cell_edit$value
      
    })
    
    # Parameters table logic ----
    
    observeEvent(input$add_parameter,
                 {
                   req(input$parameter_name,
                       r_beehave$input_parameters,
                       constant_defaults)
                   
                   parameter_name <- input$parameter_name
                   value <- constant_defaults[parameter_name][[1]]
                   
                   r_beehave$input_parameters <-
                     rbind(
                       r_beehave$input_parameters,
                       data.frame(
                         Parameter = parameter_name,
                         Value = value,
                         `Default Value` = value
                       )
                     )
                   
                   parameter_choices <-
                     setdiff(names(constant_defaults),
                             r_beehave$input_parameters$Parameter)
                   
                   updateSelectInput(
                     session = session,
                     inputId = "parameter_name",
                     selected = parameter_choices[1],
                     choices = parameter_choices
                   )
                 })
    
    # Editing
    observeEvent(input$parameters_table_cell_edit, {
      r_beehave$input_parameters[input$parameters_table_cell_edit$row,
                                 input$parameters_table_cell_edit$col] <-
        input$parameters_table_cell_edit$value
      
    })
    
    # Workflow submission logic ----
    
    listen_workflow_button_status <- reactive({
      list(
        input$map_list,
        r_beehave$lookup_table,
        input$upload_dbf,
        r_beehave$input_parameters
      )
    })
    
    observeEvent(listen_workflow_button_status(), {
      if (purrr::is_empty(input$map_list) |
          purrr::is_empty(input$upload_dbf) |
          purrr::is_empty(r_beehave$lookup_table) |
          purrr::is_empty(r_beehave$input_parameters)) {
        shinyjs::disable("run_workflow")
      } else {
        shinyjs::enable("run_workflow")
      }
    })
    
    observeEvent(input$run_workflow, {
      
      original_wd <- getwd()
      setwd(temp_dir) 
      
      lookup_file <- file.path("lookup_table.csv")
      parameters_file <- file.path("parameters.csv")
      locations_file <- file.path("locations.dbf")
      map_file <- file.path("map.tif")
      map_xml_file <- file.path("map.tif.aux.xml")
      tar_file <- file.path(temp_dir, "beehave_input.tar.gz")
      
      print(tar_file)
      
      fs::file_copy(
        file.path(beehave_maps_dir, input$map_list),
        map_file,
        overwrite = TRUE)

      fs::file_copy(
        file.path(beehave_maps_dir, paste0(input$map_list, ".aux.xml")),
        map_xml_file,
        overwrite = TRUE)

      fs::file_copy(
        input$upload_dbf$datapath,
        locations_file,
        overwrite = TRUE)
      
      write.csv(
        r_beehave$lookup_table,
        file = lookup_file)
      
      write.csv(
        r_beehave$input_parameters,
        file = parameters_file)
      
      tar(tar_file,
          files = c(locations_file,
                    map_file,
                    map_xml_file,
                    lookup_file,
                    parameters_file
                    ),
          compression = "gzip"
          )
      
      req(r$lexis_token)
      
      dataset_name <- paste('Beehave WF Input ', format(Sys.time(), "%Y%m%d%H%M%S "), stringi::stri_rand_strings(1, 5))
      
      metadata <- list(
        path = '',
        zone = 'IT4ILexisZone',
        filename = basename(tar_file),
        user = 'mar0486',
        project = 'biodt_development',
        access = 'project',
        expand = 'yes',
        encryption = 'no',
        metadata = jsonlite::toJSON(
          list(
            contributor = '',
            creator = '',
            owner = '',
            publicationYear = format(Sys.Date(), format = "%Y"),
            publisher = '',
            resourceType = 'Dataset',
            title = dataset_name
          )
        )
      )
      
      r4lexis::upload_dataset(
        file_path = tar_file,
        lexis_oauth_token = r$lexis_token,
        metadata = metadata,
        chunk_size = 1024*1024*5 
      )

      Sys.sleep(5)
      
      new_ds_list <- r4lexis::get_dataset_list(r$lexis_token)
      ds_data <- r4lexis::extract_dataset(new_ds_list, dataset_name, "metadata", "title")
      ds_workflow_input_id <- ds_data$location$internalID
      
      print(ds_workflow_input_id)
      
      setwd(original_wd)
    })
    
    # Render outputs ----
    
    output$input_map_plot <-
      leaflet::renderLeaflet(r_beehave$input_leaflet_map)
    output$lookup_table <- DT::renderDT({
      DT::datatable(
        r_beehave$lookup_table,
        editable = list(target = "cell", disable = list(columns = 1)),
        selection = "none"
      )
    })
    output$parameters_table <- DT::renderDT({
      DT::datatable(
        r_beehave$input_parameters,
        editable = list(target = "cell", disable = list(columns = c(1, 3))),
        selection = "none"
      )
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

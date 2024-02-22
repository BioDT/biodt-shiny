#' beehave UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib card card_header card_body page_fluid navset_tab nav_panel nav_spacer
#' @importFrom DT DTOutput
#' @importFrom leaflet leafletOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs hidden
#' @importFrom ggplot2 ggplot geom_line aes

mod_beehave_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_fluid(
      id = ns("page"),
      class = "p-0",
      bslib::navset_tab(
        bslib::nav_panel(
          title = "Info"
        ),
        bslib::nav_panel(
          # Beekeper Case ----
          title = "Beekeper",
          bslib::layout_column_wrap(
            width = NULL, 
            height = 800, 
            fill = FALSE,
            style = htmltools::css(grid_template_columns = "1fr 2fr"),
            # Parameters for the Beekeeper Simulation----
            bslib::card(
              title = "params_simulation",
              full_screen = TRUE,
              class = "p-3",
              card_header(
                "Parameters for the Beekeeper Simulation",
                class = "h-5"  
              ),
              bslib::card_body(
                shiny::sliderInput(
                  ns("N_INITIAL_BEES"),
                  label = "Number of adult bees at the beginning of the simulation",
                  min = 0,
                  max = 30000,
                  value = 10000,
                  step = 100
                ),
                shiny::sliderInput(
                  ns("N_INITIAL_MITES_HEALTHY"),
                  label = "Number of Mites at the beginning of the simulation",
                  value = 100,
                  min = 0,
                  max = 100,
                  step = 1
                ),
                shiny::sliderInput(
                  ns("N_INITIAL_MITES_INFECTED"),
                  label = "Number of infected Mites at the beginning of the simulation",
                  value = 100,
                  min = 0,
                  max = 100,
                  step = 1
                ),
                shinyWidgets::prettyCheckbox(
                  ns("HoneyHarvesting"),
                  label = "Honey Harvest",
                  value = TRUE,
                  icon = icon("check"),
                  status = "success",
                  animation = "smooth"
                ),
                shinyWidgets::prettyCheckbox(
                  ns("VarroaTreatment"),
                  label = "Varroa treatment with arcaricide",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success",
                  animation = "smooth"
                ),
                shinyWidgets::prettyCheckbox(
                  ns("DroneBroodRemoval"),
                  label = "Drone Brood Removal",
                  value = TRUE,
                  icon = icon("check"),
                  status = "success",
                  animation = "smooth"
                ),
              ),
            ),
            # Beekeper Input Map ----
            bslib::card(
              title = "input_map",
              id = ns("input_map"),
              full_screen = TRUE,
              class = "p-3",
              card_header(
                "Input Map",
                class = "h-5"
              ),
              bslib::card_body(
                shiny::selectInput(ns("map_list"),
                                   label = "Choose input map",
                                   choices = NULL),
                shiny::uiOutput(ns("map_coordinates"),),
                leaflet::leafletOutput(ns("input_map_plot")),
                bslib::layout_column_wrap(width = 1,
                                          shinyjs::disabled(
                                            shiny::actionButton(
                                              ns("load_resources"),
                                              label = "Load beehave resources",
                                              width = "100%",
                                              class = "btn-primary"
                                            )
                                          ))
              )
            ),
          ), 
          

        
            # Lookup Table----
          bslib::card(
            title = "lookup_table",
            full_screen = TRUE,
            card_body(
              shiny::h3("Lookup table"),
              shiny::selectInput(
                ns("lookup_list"),
                label = "Choose input lookup table",
                choices = NULL
              ),
              shinyjs::disabled(
                div(
                  DTOutput(
                    ns("lookup_table")
                  )
                )
              )
            )
          ),
        
            # Parameters Table----
          # bslib::card(
          #   title = "parameters_table",
          #   full_screen = TRUE,
          #   bslib::card_body(
          #     shiny::h3("Parameters Table"),
          #     DT::DTOutput(
          #       ns("parameters_table")
          #     )
          #   )
          # ),

          # Output Bees Plot----
          bslib::card(
            title = "output_bees",
            full_screen = TRUE,
            card_title("Output Bees Plot"),
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
              shinyjs::disabled(
                shiny::actionButton(
                  ns("update_output"),
                  label = "Show results"
                )
              ),
              # plotOutput(
              #   ns("output_bees_plot")
              # )
            ),
            
            echarty::ecs.output(
              ns("echart_pollinators_output"),
              width = "100%",
              height = "500px"
            )
          ),
        
            # Output Honey Plot----
          bslib::card(
            title = "output_honey",
            full_screen = TRUE,
            card_title("Output Honey Plot"),
            card_body(
              plotOutput(
                ns("output_honey_plot")
              )
            )
          ),
          
            # h4 run wrf action btn----
          bslib::card(
            bslib::layout_column_wrap(
              width = 1 / 2,
              div(
                shiny::h4("Workflow"),
                shiny::actionButton(
                  ns("run_workflow"),
                  label = "Run Workflow"
                ),
              ),
              div(
                shiny::h4("Instructions"),
                shiny::actionButton(
                  ns("walkthrough"),
                  "Run guided walkthrough",
                ),
              )
            )
          )
        ),

          # Expert Case ----
    bslib::nav_panel(
      title = "Expert",
      bslib::navset_underline(
        id = ns("expert_wizard"),
        bslib::nav_spacer(),
      
          # Expert Model ----
        bslib::nav_panel(
          title = "1 - Model",
          bslib::card(
            shiny::fileInput(
              inputId = ns("expert_model_file"),
              label = "Upload model file"
            ),
            shiny::selectInput(
              inputId = ns("expert_model_select"),
              label = "Select model",
              choices = ""
            ),
            shiny::actionButton(
              inputId = ns("expert_model_param"),
              label = "Next"
            )
          ),
        ),
          # Expert Parameter Space ----
        bslib::nav_panel(
          title = "2 - Parameters",
          bslib::card(
            bslib::card_body(
              shiny::actionButton(
                inputId = ns("expert_parameter_load"),
                label = "Load default parameters"
              ),
              shiny::fileInput(
                inputId = ns("expert_parameter_file"),
                label = "Upload parameter file"
              ),
              shinyjs::disabled(
                DT::DTOutput(
                  ns("expert_parameter_table")
                )
              ),
              bslib::layout_column_wrap(
                width = 1 / 2,
                shiny::actionButton(
                  inputId = ns("expert_param_model"),
                  label = "Back"
                ),
                shiny::actionButton(
                  inputId = ns("expert_param_location"),
                  label = "Next"
                )
              )
            )
          )
        ),
          # Expert Locations ----
        bslib::nav_panel(
          title = "3 - Locations",
          bslib::card(
            shiny::radioButtons(
              ns("expert_locations_type"),
              label = "Choose location type",
              choices = list(
                `Latitude, Longitude` = "latlon",
                `CSV file` = "csv",
                `Custom input file` = "custom"
              ),
              selected = "latlon"
            ),
            shiny::div(
              id = ns("expert_locations_latlon"),
              bslib::layout_column_wrap(
                width = 1 / 3,
                shiny::numericInput(
                  ns("lat"),
                  label = "Latitude",
                  value = 51.3919
                ),
                shiny::numericInput(
                  ns("lon"),
                  label = "Longitude",
                  value = 11.8787
                )
              )
            ),
            shinyjs::disabled(
              shiny::fileInput(
                inputId = ns("expert_locations_file"),
                label = "Upload locations file"
              )
            )
          ), 
          bslib::card(
            title = "expert_locations_map",
            full_screen = TRUE,
            card_title("Locations Map"),
            card_body(
              leaflet::leafletOutput(
                ns("expert_locations_map_plot")
              )
            )
          ),
          bslib::layout_column_wrap(
            width = 1 / 2,
            shiny::actionButton(
              inputId = ns("expert_location_param"),
              label = "Back"
            ),
            shiny::actionButton(
              inputId = ns("expert_location_weather"),
              label = "Next"
            )
          )
        ),
          # Expert Weather ----
        bslib::nav_panel(
          title = "4 - Weather",
          bslib::card(
            shiny::radioButtons(
              ns("expert_weather_provider"),
              label = "Choose weather provider",
              choices = list(
                "RDWD", "Custom file"
             ),
              selected = "RDWD"
            ),
            shinyjs::disabled(
              shiny::fileInput(
              inputId = ns("expert_weather_file"),
              label = "Upload weather file"
              )
            )
          ),
          bslib::layout_column_wrap(
            width = 1 / 2,
            shiny::actionButton(
              inputId = ns("expert_weather_location"),
              label = "Back"
            ),
            shiny::actionButton(
              inputId = ns("expert_weather_execution"),
              label = "Next"
            )
          )
        ),
          # Expert Execution ----
        bslib::nav_panel(
          title = "5 - Execution",
          bslib::card(
            bslib::card_body(
              shiny::uiOutput(
                outputId = ns("expert_execution_summary"),
                class = "p-1"
              ),
              shinyjs::disabled(
                DT::DTOutput(
                  ns("expert_execution_table")
                )
              ),
              shinyjs::disabled(
                shiny::actionButton(
                  inputId = ns("expert_execution_run"),
                  label = "Run Workflow"
                )
              ),
              shiny::actionButton(
                inputId = ns("expert_execution_weather"),
                label = "Back"
              )
            )
          ),
          bslib::nav_spacer()
        )
    


  
        # Grass Cutting Case ----
          # bslib::nav_panel(title = "Grass Cutting"),
          # Urban Planning Case ----
          # bslib::nav_panel(title = "Urban Planninng")
      )
    )
    
      ) ### END navset_tab
    )   ### END page_fluid
  )     ### END tagList
}

#' beehave Server Functions
#'
#' @importFrom DT renderDT
#' @importFrom shinipsum random_DT random_ggplot
#' @importFrom shinyjs showElement hideElement
#' @importFrom r4lexis download_file_from_dataset extract_dataset get_dataset_file_list
#' @importFrom terra rast plet
#' @importFrom purrr map_chr pluck
#' @importFrom cli hash_md5
#' @importFrom leaflet renderLeaflet
#' @importFrom leaflet.extras addDrawToolbar drawMarkerOptions
#' @importFrom golem print_dev
#' @importFrom cicerone Cicerone
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
      input_parameters = init_input_parameters_beehave(),
      output_datasets_list = NULL,
      output_datasets_names = NULL,
      output_ds = NULL,
      output_data = NULL,
      output_last_dataset = "",
      expert_param_table = NULL,
      feature = NULL,
      coordinates_text = HTML('<p style = "color: red">No place selected.<br>Use point selector in the map below.</p>'),
      # TODO for future commented lines below ----
      # N_INITIAL_BEES = 10000,
      # N_INITIAL_MITES_HEALTHY = 33,
      # N_INITIAL_MITES_INFECTED = 32,
      # HoneyHarvesting = TRUE,
      # VarroaTreatment = FALSE,
      # DroneBroodRemoval = TRUE
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
    beehave_output_dir <-
      file.path(temp_dir, "output")
    if (!dir.exists(beehave_output_dir)) {
      dir.create(beehave_output_dir)
    }
    # constant_defaults <- init_const()
    
    # Path to stored files (should be set for where the files are stored)
    pollinators_path <- golem::get_golem_options("pollinators_path")
    
    # Load maps and lookup table list ----
    # Helper function to observe multiple events
    listen_maps_input <- reactive({
      list(r$page_name)
    })
    
    # Load offline data ----
    observeEvent(r$page_name,
                 {
                   req(r$page_name == "Pollinators")
                   
                   if (is.null(r_beehave$input_rast_map)) {
                     r_beehave$input_rast_map <-
                       terra::rast(file.path(pollinators_path, "map.tif"))
                     r_beehave$input_leaflet_map <-
                       terra::plet(r_beehave$input_rast_map,
                                   tiles = "Streets",
                                   alpha = 0.4) |>       
                       leaflet.extras::addDrawToolbar(         
                         polylineOptions = FALSE,         
                         polygonOptions = FALSE,         
                         circleOptions = FALSE,         
                         rectangleOptions = FALSE,
                         markerOptions = leaflet.extras::drawMarkerOptions(), 
                         circleMarkerOptions = FALSE, 
                         singleFeature = TRUE)     
                   }
                   
                   if (is.null(r_beehave$lookup_table)) {
                     r_beehave$lookup_table <-
                       readr::read_csv(file.path(pollinators_path, "lookup_table.csv"),
                                       show_col_types = FALSE)
                   }
                   
                   output$echart_pollinators_output <- 
                     echarty::ecs.render(
                       beekeeper_output_plot(
                         file.path(pollinators_path, "output_example/Result_table_original.csv"),
                         file.path(pollinators_path, "output_example/weather_412.txt"),
                       )
                     )
                 })
    
    observeEvent(r$page_name,
                 {
                   req(r$lexis_token,
                       r$lexis_dataset_list,
                       r$page_name == "Pollinators")
                   
                   r_beehave$map_ds <-
                     r4lexis::extract_dataset(r$lexis_dataset_list,
                                              r$beehave_map_dataset,
                                              "metadata",
                                              "title")
                   if ("lexis_ds_list" %in% class(r_beehave$map_ds)) {
                     r_beehave$map_ds <- r_beehave$map_ds[[1]]
                   }
                   
                   golem::print_dev("Getting Beehave Map dataset file list.")
                   r_beehave$map_files <-
                     r4lexis::get_dataset_file_list(
                       r$lexis_token,
                       internalID = r_beehave$map_ds$location$internalID,
                       project = r$beehave_user_project
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
                   
                   # Show download resources button if they are not downloaded
                   if ((length(r_beehave$input_map_list) > 0 &
                        r_beehave$maps_loaded == FALSE) |
                       (length(r_beehave$input_lookup_list) > 0 &
                        r_beehave$lookup_loaded == FALSE)) {
                     golem::print_dev("Showing resources loading button.")
                     shinyjs::showElement("load_resources")
                   }
                   
                   r_beehave$lookup_ds <-
                     r4lexis::extract_dataset(r$lexis_dataset_list,
                                              r$beehave_lookup_dataset,
                                              "metadata",
                                              "title")
                   
                   if ("lexis_ds_list" %in% class(r_beehave$lookup_ds)) {
                     r_beehave$lookup_ds <- r_beehave$lookup_ds[[1]]
                   }
                   
                   golem::print_dev("Getting Beehave Lookup table file list.")
                   r_beehave$lookup_files <-
                     r4lexis::get_dataset_file_list(
                       r$lexis_token,
                       internalID = r_beehave$lookup_ds$location$internalID,
                       project = r$beehave_user_project
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
                   
                   # Update result datasets ----
                   r_beehave$output_datasets_list <-
                     r4lexis::extract_dataset(r$lexis_dataset_list,
                                              "Beehave WF Output ",
                                              "metadata",
                                              "title")
                   
                   r_beehave$output_datasets_names <-
                     r_beehave$output_datasets_list |>
                     purrr::map(purrr::pluck, "metadata", "title") |>
                     unlist()
                   
                   updateSelectInput(
                     session = session,
                     inputId = "output_list",
                     selected = r_beehave$output_datasets_names[1],
                     choices = r_beehave$output_datasets_names
                   )
                 })
    
    # Render Leaflet map - with draw marker option ON ----
    r_beehave$input_leaflet_map <-      
      leaflet::leaflet() |>       
        leaflet::addTiles() |>       
        leaflet.extras::addDrawToolbar(         
        polylineOptions = FALSE,         
        polygonOptions = FALSE,         
        circleOptions = FALSE,         
        rectangleOptions = FALSE,
        markerOptions = leaflet.extras::drawMarkerOptions(), 
        circleMarkerOptions = FALSE, 
        singleFeature = TRUE)     
    
    observeEvent(input$input_map_plot_draw_new_feature, {
      r_beehave$feature <- input$input_map_plot_draw_new_feature
      # print(r_beehave$feature)
    })

    # Leaflet map - render coordinates ----
    output$map_coordinates <-
      shiny::renderUI(r_beehave$coordinates_text)
    
    shiny::observeEvent(r_beehave$feature,
                        {
                          shiny::req(r_beehave$feature)
                          golem::print_dev(class(r_beehave$feature$geometry$coordinates))
                          
                          if (is.null(r_beehave$feature)) {
                            r_beehave$coordinates_text <- "No place selected."
                          } else {
                            r_beehave$coordinates_text  <- HTML(paste(
                              "Selected coordinates are: <br>",
                              "Latitude: ", r_beehave$feature$geometry$coordinates[[2]], "<br>",
                              "Longitude: ", r_beehave$feature$geometry$coordinates[[1]], "<br>"
                              ))
                          }
                        })
    
    
    # Output dataset logic ----
    observeEvent(input$output_list,
                 {
                   req(r_beehave$output_datasets_list,
                       r_beehave$output_datasets_names)
                   output_ind <-
                     which(r_beehave$output_datasets_names == input$output_list)
                   
                   r_beehave$output_ds <-
                     r_beehave$output_datasets_list[[output_ind]]
                   output_uuid <-
                     r_beehave$output_datasets_list[[output_ind]] |>
                     purrr::pluck("location",
                                  "internalID")
                   
                   golem::print_dev("Getting Beehave Output dataset file list.")
                   r_beehave$output_files <-
                     r4lexis::get_dataset_file_list(
                       r$lexis_token,
                       internalID = output_uuid,
                       project = r$beehave_user_project
                     )
                   
                   r_beehave$output_list <-
                     r_beehave$output_files$contents |>
                     purrr::map_chr(purrr::pluck, "name")
                   
                   updateSelectInput(
                     session = session,
                     inputId = "output_files_list",
                     selected = r_beehave$output_list[1],
                     choices = r_beehave$output_list
                   )
                 },
                 ignoreInit = TRUE)
    
    observeEvent(input$output_files_list,
                 {
                   if (length(input$output_files_list) > 0) {
                     shinyjs::enable("update_output")
                   } else {
                     shinyjs::disable("update_output")
                   }
                 })
    
    observeEvent(input$update_output,
                 {
                   req(input$output_files_list,
                       input$output_list)
                   
                   golem::print_dev("Update outputs button pressed.")
                   # Delete files if dataset changed and download new ones
                   if (r_beehave$output_last_dataset != input$output_list) {
                     file.remove(list.files(beehave_output_dir,
                                            full.names = TRUE))
                     
                     golem::print_dev("Donwloading Beehave Output dataset files.")
                     r4lexis::download_file_from_dataset(
                       r$lexis_token,
                       r_beehave$output_ds,
                       destination_path = beehave_output_dir,
                       extract = TRUE
                     )
                   }
                   
                   print(input$output_files_list)
                   r_beehave$output_data <-
                     purrr::map_dfr(file.path(beehave_output_dir,
                                              input$output_files_list),
                                    read.csv) |>
                     tibble::as_tibble()
                   
                   r_beehave$output_last_dataset <-
                     input$output_list
                 })
    
    # Download maps from Lexis DDI ----
    
    observeEvent(input$load_resources,
                 {
                   req(r$lexis_token)
                   
                   # Map part ----
                   non_dev <- TRUE
                   if (length(list.files(beehave_maps_dir)) > 0 &
                       golem::app_dev()) {
                     r_beehave$maps_loaded <- TRUE
                     shinyjs::hideElement("load_resources")
                     shinyjs::showElement("lookup_table")
                     shinyjs::showElement("input_map_plot")
                     non_dev <- NULL
                   }
                   
                   if (length(list.files(beehave_lookup_dir)) > 0 &
                       golem::app_dev()) {
                     r_beehave$lookup_loaded <- TRUE
                     non_dev <- NULL
                   }
                   
                   req(non_dev,
                       cancelOutput = TRUE)
                   golem::print_dev("Downloading Beehave Map files.")
                   r4lexis::download_file_from_dataset(
                     r$lexis_token,
                     r_beehave$map_ds,
                     destination_path = beehave_maps_dir,
                     extract = TRUE
                   )
                   
                   if (length(list.files(beehave_maps_dir)) > 0) {
                     golem::print_dev("Beehave Map files downloaded.")
                     r_beehave$maps_loaded <- TRUE
                     shinyjs::showElement("input_map_plot")
                   } else {
                     golem::print_dev("Input maps NOT downloaded.")
                   }
                   
                   # Lookup part ----
                   
                   golem::print_dev("Downloading Beehave Lookup table files.")
                   r4lexis::download_file_from_dataset(
                     r$lexis_token,
                     r_beehave$lookup_ds,
                     destination_path = beehave_lookup_dir,
                     extract = TRUE
                   )
                   
                   if (length(list.files(beehave_lookup_dir)) > 0) {
                     golem::print_dev("Beehave Lookup tables downloaded.")
                     shinyjs::showElement("lookup_table")
                     r_beehave$lookup_loaded <- TRUE
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
                                 alpha = 0.4) |>       
                     leaflet.extras::addDrawToolbar(         
                       polylineOptions = FALSE,         
                       polygonOptions = FALSE,         
                       circleOptions = FALSE,         
                       rectangleOptions = FALSE,
                       markerOptions = leaflet.extras::drawMarkerOptions(), 
                       circleMarkerOptions = FALSE, 
                       singleFeature = TRUE)     
                 })
    
    # Lookup tables logic ----
    
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
    
    # Logic of Parameters for the Beekeeper Simulation----
    ## Numeric inputs presented as sliders
    observeEvent(input$N_INITIAL_BEES,
                 {
                   req(input$N_INITIAL_BEES)
                   # golem::print_dev(paste0("N_INITIAL_BEES: ", input$N_INITIAL_BEES))
                 })
    
    ## Update MAX of sliderInput "N_INITIAL_MITES_INFECTED"
    ## when value of sliderInput "N_INITIAL_MITES_HEALTHY" changes
    observeEvent(input$N_INITIAL_MITES_HEALTHY,
                 ignoreInit = TRUE,
                 {
                   shiny::updateSliderInput(
                     inputId = "N_INITIAL_MITES_INFECTED",
                     max = input$N_INITIAL_MITES_HEALTHY
                   )
                 })
    ## checkboxes (TRUE/FALSE)
    observeEvent(input$HoneyHarvesting,
                 {
                   # golem::print_dev(paste0("HoneyHarvesting: ", input$HoneyHarvesting))
                 })
    observeEvent(input$VarroaTreatment,
                 {
                   # golem::print_dev(paste0("VarroaTreatment: ", input$VarroaTreatment))
                 })
    observeEvent(input$DroneBroodRemoval,
                 {
                   # golem::print_dev(paste0("DroneBroodRemoval: ", input$DroneBroodRemoval))
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
      req(r$beehave_user_project)
      
      project_ddi_id <- paste0("proj",
                               cli::hash_md5(r$beehave_user_project))
      
      original_wd <- getwd()
      setwd(temp_dir)
      
      lookup_file <- file.path("lookup_table.csv")
      parameters_file <- file.path("parameters.csv")
      locations_file <- file.path("locations.dbf")
      map_file <- file.path("map.tif")
      map_xml_file <- file.path("map.tif.aux.xml")
      tar_file <- file.path(temp_dir, "beehave_input.tar.gz")
      
      print(tar_file)
      
      fs::file_copy(file.path(beehave_maps_dir, input$map_list),
                    map_file,
                    overwrite = TRUE)
      
      fs::file_copy(file.path(beehave_maps_dir, paste0(input$map_list, ".aux.xml")),
                    map_xml_file,
                    overwrite = TRUE)
      
      fs::file_copy(input$upload_dbf$datapath,
                    locations_file,
                    overwrite = TRUE)
      
      write.csv(r_beehave$lookup_table,
                file = lookup_file,
                row.names = FALSE)
      
      write.csv(r_beehave$input_parameters,
                file = parameters_file,
                row.names = FALSE)
      
      # Compress everything into tar that can be uploaded to LEXIS DDI
      tar(
        tar_file,
        files = c(
          locations_file,
          map_file,
          map_xml_file,
          lookup_file,
          parameters_file
        ),
        compression = "gzip"
      )
      
      req(r$lexis_token)
      
      # Prepare variables for LEXIS Workflow
      run_id <- stringi::stri_rand_strings(1, 5)
      dataset_name <-
        paste('Beehave WF Input ',
              format(Sys.time(), "%Y%m%d%H%M%S "),
              run_id)
      
      metadata <- list(
        path = '',
        zone = 'IT4ILexisZone',
        filename = basename(tar_file),
        user = r$user_info$preferred_username,
        project = r$beehave_user_project,
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
      
      golem::print_dev("Uploading Beehave Workflow dataset.")
      r4lexis::upload_dataset(
        file_path = tar_file,
        lexis_oauth_token = r$lexis_token,
        metadata = metadata,
        chunk_size = 1024 * 1024 * 5
      )
      
      Sys.sleep(5)
      
      # These lines are used to get the uploaded dataset UUID
      new_ds_list <- r4lexis::get_dataset_list(r$lexis_token)
      ds_data <-
        r4lexis::extract_dataset(new_ds_list, dataset_name, "metadata", "title")
      ds_workflow_input_id <- ds_data$location$internalID
      
      setwd(original_wd)
      golem::cat_dev(
        "\nFinished dataset upload.",
        "\nDataset UUID: ",
        ds_workflow_input_id,
        "\n\n",
        sep = ""
      )
      
      golem::print_dev("Getting Beehave workflow default parameters.")
      input_defaults <-
        r4lexis::get_workflow_default_parameters(r$lexis_token,
                                                 r$beehave_workflow_id,
                                                 verbosity = 0)
      
      golem::print_dev("Read default workflow inputs.")
      # Hardcoded version of cloud image to use
      input_defaults$cloud_image <-
        "nithador/biodt-beehave-data:v0.1.18"
      input_defaults$input_dataset_id <-
        ds_workflow_input_id
      input_defaults$input_locations <-
        paste0("/beehave-input/",
               ds_workflow_input_id,
               "/locations.dbf")
      input_defaults$input_dataset_path <-
        paste0("project/",
               project_ddi_id,
               "/",
               ds_workflow_input_id)
      input_defaults$output_dataset_ddi_metadata$title <-
        paste0(paste(
          'Beehave WF Output ',
          format(Sys.time(), "%Y%m%d%H%M%S "),
          run_id
        ))
      
      
      
      golem::print_dev("Executing Beehave Workflow.")
      resp <- r4lexis::execute_workflow(
        r$lexis_token,
        r$beehave_workflow_id,
        input_defaults,
        paste0(
          "BeehaveShiny",
          format(Sys.time(), format = "%Y%m%d%H%M%S"),
          "-",
          run_id
        )
      )
      
      golem::print_dev("Ran new workflow.")
      
    })
    
    # Render outputs ----
    
    output$input_map_plot <-
      leaflet::renderLeaflet(r_beehave$input_leaflet_map)
    
    output$lookup_table <- DT::renderDT({
      DT::datatable(
        r_beehave$lookup_table,
        editable = list(target = "cell", disable = list(columns = c(0, 1))),
        selection = "none",
        options = list(scrollX = TRUE),
        class = c("hover", "compact")
      )
    })
    
    output$parameters_table <- DT::renderDT({
      DT::datatable(
        r_beehave$input_parameters,
        editable = list(target = "cell", disable = list(columns = c(0, 1, 3))),
        selection = "none",
        options = list(scrollX = TRUE),
        class = c("hover", "compact"),
        fillContainer = FALSE,
        height = "100%"
      )
    })
    
    output$output_bees_plot <- renderPlot({
      req(r_beehave$output_data)
      ggplot2::ggplot(r_beehave$output_data) +
        ggplot2::geom_line(
          ggplot2::aes(x = `X.step.`,
                       y = `TotalIHbees...TotalForagers`,
                       color = WeatherFile)
        ) +
        ggplot2::xlab("Total Bees + Foragers") +
        ggplot2::ylab("Timestep") +
        ggplot2::theme_minimal()
    })
    
    output$output_honey_plot <- renderPlot({
      req(r_beehave$output_data)
      ggplot2::ggplot(r_beehave$output_data) +
        ggplot2::geom_line(
          ggplot2::aes(x = `X.step.`,
                       y = `X.honeyEnergyStore.....ENERGY_HONEY_per_g...1000...`,
                       color = WeatherFile)
        ) +
        ggplot2::xlab("Honey Energy Store") +
        ggplot2::ylab("Timestep") +
        ggplot2::theme_minimal()
    })
    
    output$output_map_plot <- renderPlot({
      random_ggplot(type = "hex")
    })
    
    # Expert Navigation ----
    observeEvent(input$expert_model_param,
                 {
                   bslib::nav_select("expert_wizard",
                                     "2 - Parameters")
                 })
    
    observeEvent(input$expert_param_model,
                 {
                   bslib::nav_select("expert_wizard",
                                     "1 - Model")
                 })
    
    observeEvent(input$expert_param_location,
                 {
                   bslib::nav_select("expert_wizard",
                                     "3 - Locations")
                 })
    
    observeEvent(input$expert_location_param,
                 {
                   bslib::nav_select("expert_wizard",
                                     "2 - Parameters")
                 })
    
    observeEvent(input$expert_location_weather,
                 {
                   bslib::nav_select("expert_wizard",
                                     "4 - Weather")
                 })
    
    observeEvent(input$expert_weather_location,
                 {
                   bslib::nav_select("expert_wizard",
                                     "3 - Locations")
                 })
    
    observeEvent(input$expert_weather_execution,
                 {
                   bslib::nav_select("expert_wizard",
                                     "5 - Execution")
                 })
    
    observeEvent(input$expert_execution_weather,
                 {
                   bslib::nav_select("expert_wizard",
                                     "4 - Weather")
                 })
    # Expert Model logic ----
    # Expert Parameters logic ----
    # Parameters DT
    
    observeEvent(input$expert_parameter_load,
                 {
                   r_beehave$expert_param_table <- shinipsum::random_DT(ncol = 4,
                                                                        nrow = 15)
                 })
    
    output$expert_parameter_table <-
      DT::renderDataTable(r_beehave$expert_param_table)
    
    # Expert Locations logic ----
    # Input selection
    observeEvent(input$expert_locations_type,
                 ignoreInit = TRUE,
                 {
                   file_switch <- any(
                     c(input$expert_locations_type == "csv",
                       input$expert_locations_type == "custom")
                   )
                   shinyjs::toggle(id = "expert_locations_latlon",
                                   condition = input$expert_locations_type == "latlon")
                   shinyjs::toggle(id = "expert_locations_file",
                                   condition =  file_switch)
                 })
    # Map
    output$expert_locations_map_plot <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::setView(lng = 11.8787,
                         lat = 51.3919,
                         zoom = 9) |>
        leaflet::addMarkers(lng = 11.8787,
                            lat = 51.3919)
    })
    # Expert Weather logic ----
    observeEvent(input$expert_weather_provider,
                 ignoreInit = TRUE,
                 {
                   shinyjs::toggle(id = "expert_weather_file",
                                   condition = input$expert_weather_provider == "Custom file")
                 })
    # Expert Execution logic ----
    output$expert_execution_summary <-
      shiny::renderUI(tagList(
        shiny::HTML(
          "<table>
           <tr>",
          "<th><b>Model               </b></th><td>:</td><td>",
          input$expert_model_select,
          "</td></tr><tr>",
          "<th><b>Locations type      </b></th><td>:</td><td>",
          input$expert_locations_type,
          "</td></tr><tr>",
          "<th><b>Weather input type  </b></th><td>:</td><td>",
          input$expert_weather_type,
          "</td></tr>
          </table>"
        )
      ))
    
    output$expert_execution_table <-
      DT::renderDT(r_beehave$expert_param_table)
    
    observeEvent(r_beehave$expert_param_table,
                 {
                   shinyjs::toggle(
                     id = "expert_execution_table",
                     condition = !is.null(r_beehave$expert_param_table)
                   )
                   shinyjs::toggle(
                     id = "expert_parameter_table",
                     condition = !is.null(r_beehave$expert_param_table)
                   )
                 })
    
    # Set up Cicerone library for walkthrough----
    guide <- cicerone::Cicerone$
      new(allow_close = TRUE)$
      step(
        ns("page"),
        "Beehave Sidebar Controls",
        "Ok, friends, let's see what we have here. In the begining you'll probably need to choose a map...",
        position = "right"
      )$
      step(
        ns("map_input"),
        "Input Map",
        "For example, it can be done by choosing input map via select input below...",
        position = "bottom"
      )
    
    # Run walkthrough via action button "Instructions"----
    observeEvent(
      input$walkthrough,
      guide$init(session = session)$start()  
    )
    
  })
}

## To be copied in the UI
# mod_beehave_ui("beehave_1")

## To be copied in the server
# mod_beehave_server("beehave_1")

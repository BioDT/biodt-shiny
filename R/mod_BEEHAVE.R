#' beehave UI Function
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
mod_beehave_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::page_fluid(
      class = "p-0",
    bslib::layout_sidebar(
      border = FALSE,
    # bslib::page_fluid(
      # theme = biodt_theme,
      sidebar = bslib::sidebar(
      div(shinyjs::hidden(
        shiny::actionButton(
          ns("load_resources"),
          label = "Load beehave resources",
          width = "100%",
          class = "btn-primary"
        )
      )),
      shiny::h3("Map"),
      shiny::selectInput(ns("map_list"),
                         label = "Choose input map",
                         choices = NULL),
      shiny::fileInput(ns("upload_dbf"),
                       label = "Upload dbf locations file",
                       accept = ".dbf"),
      shiny::h3("Lookup table"),
      shiny::selectInput(ns("lookup_list"),
                         label = "Choose input lookup table",
                         choices = NULL),
      shiny::h3("Workflow"),
      shinyjs::disabled(shiny::actionButton(ns("run_workflow"),
                                            label = "Run Workflow")),
      ),
      bslib::card(
        title = "input_map",
        full_screen = TRUE,
        card_title("Input Map"),
        card_body(shinyjs::hidden(leaflet::leafletOutput(
          ns("input_map_plot")
        )))
      ),
      bslib::card(
        title = "lookup_table",
        full_screen = TRUE,
        card_title("Lookup Table"),
        card_body(shinyjs::hidden(DTOutput(ns("lookup_table"))))
      ),
      bslib::card(
        title = "parameters_table",
        full_screen = TRUE,
        card_title("Parameters Table"),
        card_body(DTOutput(ns(
          "parameters_table"
        )))
      ),
      bslib::card(
        title = "output_bees",
        full_screen = TRUE,
        card_title("Output Bees Plot"),
        bslib::layout_column_wrap(
          width = 1/3,
        shiny::selectInput(ns("output_list"),
                           label = "Choose output dataset",
                           choices = NULL),
        shiny::selectInput(ns("output_files_list"),
                           label = "Choose output files",
                           choices = NULL,
                           multiple = TRUE),
        shinyjs::disabled(shiny::actionButton(ns("update_output"),
                                              label = "Show results"))
        ),
        plotOutput(ns("output_bees_plot"))
      ),
      bslib::card(
        title = "output_honey",
        full_screen = TRUE,
        card_title("Output Honey Plot"),
        card_body(plotOutput(ns(
          "output_honey_plot"
        )))
      ),
      # bslib::card(
      #   title = "output_map",
      #   full_screen = TRUE,
      #   card_title("Output Map"),
      #   card_body(plotOutput(ns(
      #     "output_map_plot"
      #   )))
      # )
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
      input_parameters = init_input_parameters_beehave(),
      output_datasets_list = NULL,
      output_datasets_names = NULL,
      output_ds = NULL,
      output_data = NULL,
      output_last_dataset = ""
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
                   
                   golem::print_dev("Getting Beehave Map dataset file list.")
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
                                              "Beehave Input Lookup",
                                              "metadata",
                                              "title")
                   
                   golem::print_dev("Getting Beehave Lookup table file list.")
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
                   
                   # Update result datasets ----
                   
                   r_beehave$output_datasets_list <- r4lexis::extract_dataset(r$lexis_dataset_list,
                                                   "Beehave WF Output ",
                                                   "metadata",
                                                   "title")
                   
                   r_beehave$output_datasets_names <- r_beehave$output_datasets_list |>
                     purrr::map(purrr::pluck, "metadata", "title") |>
                     unlist()
                   
                   updateSelectInput(
                     session = session,
                     inputId = "output_list",
                     selected = r_beehave$output_datasets_names[1],
                     choices = r_beehave$output_datasets_names
                   )
                   
                 })
    # Output dataset logic ----
    
    observeEvent(
      input$output_list,
      {
        req(r_beehave$output_datasets_list,
            r_beehave$output_datasets_names)
        output_ind <- which(r_beehave$output_datasets_names == input$output_list)
        
        r_beehave$output_ds <- r_beehave$output_datasets_list[[output_ind]]
        output_uuid <- r_beehave$output_datasets_list[[output_ind]] |>
          purrr::pluck("location",
                       "internalID")
        
        golem::print_dev("Getting Beehave Output dataset file list.")
        r_beehave$output_files <-
          r4lexis::get_dataset_file_list(
            r$lexis_token,
            internalID = output_uuid,
            project = "biodt_development"
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
      ignoreInit = TRUE
    )
    
    observeEvent(
      input$output_files_list,
      {
        if (length(input$output_files_list) > 0) {
          shinyjs::enable("update_output")
        } else {
          shinyjs::disable("update_output")
        }
      }
    )
    
    observeEvent(
      input$update_output,
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
        r_beehave$output_data <- purrr::map_dfr(file.path(beehave_output_dir,
                             input$output_files_list),
                   read.csv) |>
          tibble::as_tibble()
        
        r_beehave$output_last_dataset <- input$output_list
      }
    )
    
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
                                 alpha = 0.4)
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
    
    # Parameters table logic ----
    
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
                file = lookup_file)
      
      write.csv(r_beehave$input_parameters,
                file = parameters_file)
      
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
        # actually username is needed only for user access data and should be removed in future version of lexis altogether since it knows the username from token... implemented if needed in the meantime
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
      
      golem::print_dev("Uploading Beehave Workflow dataset.")
      r4lexis::upload_dataset(
        file_path = tar_file,
        lexis_oauth_token = r$lexis_token,
        metadata = metadata,
        chunk_size = 1024 * 1024 * 5
      )
      
      Sys.sleep(5)
      
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
                                                 "biodt_beehave_karolina",
                                                 verbosity = 0)
      
      golem::print_dev("Read default workflow inputs.")
      # Hardcoded version of cloud image to use
      input_defaults$cloud_image <-
        "nithador/biodt-beehave-data:v0.1.18"
      input_defaults$input_dataset_id <-
        stringr::str_replace(
          input_defaults$input_dataset_id,
          "6cb904b4-4195-11ee-9dd3-fa163e515f81",
          ds_workflow_input_id
        )
      input_defaults$input_dataset_path <-
        stringr::str_replace(
          input_defaults$input_dataset_path,
          "6cb904b4-4195-11ee-9dd3-fa163e515f81",
          ds_workflow_input_id
        )
      input_defaults$input_locations <-
        stringr::str_replace(
          input_defaults$input_locations,
          "6cb904b4-4195-11ee-9dd3-fa163e515f81",
          ds_workflow_input_id
        )
      input_defaults$input_locations <-
        stringr::str_replace(input_defaults$input_locations,
                             "20lines",
                             "locations")
      input_defaults$output_dataset_ddi_metadata$title <-
        paste0(paste(
          'Beehave WF Output ',
          format(Sys.time(), "%Y%m%d%H%M%S "),
          run_id
        ))
      
      golem::print_dev("Executing Beehave Workflow.")
      resp <- r4lexis::execute_workflow(
        r$lexis_token,
        "biodt_beehave_karolina",
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
        class = c("hover", "compact")
      )
    })
    
    output$output_bees_plot <- renderPlot({
      req(r_beehave$output_data)
      ggplot2::ggplot(r_beehave$output_data) +
        ggplot2::geom_line(ggplot2::aes(x = `X.step.`,
                      y = `TotalIHbees...TotalForagers`,
                      color = WeatherFile)) +
        ggplot2::xlab("Total Bees + Foragers") +
        ggplot2::ylab("Timestep") +
        ggplot2::theme_minimal()
    })
    
    output$output_honey_plot <- renderPlot({
      req(r_beehave$output_data)
      ggplot2::ggplot(r_beehave$output_data) +
        ggplot2::geom_line(ggplot2::aes(x = `X.step.`,
                      y = `X.honeyEnergyStore.....ENERGY_HONEY_per_g...1000...`,
                      color = WeatherFile)) +
        ggplot2::xlab("Honey Energy Store") +
        ggplot2::ylab("Timestep") +
        ggplot2::theme_minimal()
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

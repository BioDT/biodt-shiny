# External packages
box::use(
  shiny[
    moduleServer,
    NS,
    renderUI,
    observe,
    observeEvent,
    isolate,
    reactiveVal,
    reactiveValues,
    req,
    reactive,
    renderPlot,
    invalidateLater,
    updateSliderInput,
    updateTextInput,
    withProgress,
    incProgress,
    removeUI,
    actionButton,
    column,
    fluidRow,
    selectInput,
    radioButtons,
    insertUI,
    uiOutput,
    renderText,
    updateDateRangeInput,
    updateSelectInput,
    dateRangeInput,
    icon,
    tagList,
    tags,
    div,
    conditionalPanel,
    plotOutput,
    textOutput,
    p,
    br,
    hr,
    HTML,
    wellPanel,
    checkboxInput,
    numericInput,
    helpText
  ],

  # Base R functions needed
  stats[setNames, na.omit, quantile, median, sd, var],

  # HTML structure
  htmltools[
    a,
    div,
    em,
    HTML,
    img,
    p,
    br,
    h1,
    h2,
    h3,
    h4,
    h5,
    h6,
    strong,
    tagList,
    span,
    hr,
    tags,
    pre
  ],

  # Rhinoverse ecosystem UI components
  bslib[
    card,
    card_header,
    card_body,
    layout_sidebar,
    sidebar
  ],

  # Modern Shiny components
  shinyjs[useShinyjs, runjs, extendShinyjs, disable, enable, toggleState, hidden],

  # Data manipulation with tidyverse
  dplyr[
    bind_rows, mutate, across, all_of, distinct, arrange, filter, select,
    summarise, group_by, pull, n, slice_max, rename, left_join
  ],

  # Data tidying
  tidyr[
    replace_na
  ],

  # File handling with arrow (Apache Arrow)
  arrow[read_parquet, write_parquet, Schema, schema],

  # Spatial data handling
  sf[st_read, st_as_sfc, st_bbox, st_transform, st_crs],

  # Basic utilities
  utils[head, tail, str],
  jsonlite[fromJSON, toJSON],
  fs[dir_exists],
  glue[glue],
  config[get],
  DT[DTOutput, renderDT, datatable],
  leaflet[
    leaflet, leafletOutput, renderLeaflet, addTiles, fitBounds, clearBounds,
    addMarkers, markerClusterOptions, clearMarkers, addPolygons, addLegend,
    addLayersControl, layersControlOptions, addScaleBar, flyTo, awesomeIcons,
    makeAwesomeIcon
  ],
  tibble[tibble, is_tibble],
  lubridate[as_date, ymd, today, days],
  purrr[discard, map, map_dfr, map_chr, safely, possibly],
)

# Local modules
box::use(
  app/logic/rtbm/rtbm_data_handlers[load_bird_species_info, load_parquet_data, preload_summary_data],
  app/logic/rtbm/rtbm_summary_plots[create_summary_plots, create_top_species_rank_plot, create_top_species_table_data],
  app/view/rtbm/rtbm_map[map_module_ui, map_module_server],
  app/view/rtbm/rtbm_sidebar[rtbm_sidebar_ui, rtbm_sidebar_server],
)

#' Real-time Bird Monitoring UI Module
#'
#' @param id The module ID
#' @param i18n Internationalization function
#'
#' @return A Shiny UI definition
#' @export
rtbm_app_ui <- function(id, i18n) {
  ns <- NS(id)

  # Create base layout using htmltools
  base_layout <- div(
    class = "rtbm-container container-fluid p-3",
    div(
      class = "row g-3",
      # Sidebar Toggle Button (visible on mobile)
      div(
        class = "col-12 d-md-none mb-2",
        actionButton(
          inputId = ns("toggleSidebar"),
          label = "Toggle Controls",
          icon = icon("bars"),
          class = "btn btn-secondary w-100"
        )
      ),
      # Control Panel Sidebar
      div(
        id = ns("sidebarCol"),
        class = "col-md-3 sidebar-column",
        div(
          class = "control-panel card h-100",
          # Card Header now includes collapse button directly
          div(
            class = "card-header d-flex justify-content-between align-items-center",
            span("Bird Observation Controls"),
            actionButton(
              inputId = ns("collapseSidebar"), # Keep collapse button here
              label = NULL,
              icon = icon("chevron-left"),
              class = "btn btn-sm btn-outline-secondary collapse-sidebar-btn"
            )
          ),
          # Call Sidebar Module UI (contains card body)
          rtbm_sidebar_ui(ns("sidebar"))
        )
      ),
      # Collapsed sidebar state - only shows expand button
      div(
        id = ns("collapsedSidebar"), # Keep collapsed state div here
        class = "col-auto sidebar-collapsed d-none",
        actionButton(
          inputId = ns("expandSidebar"), # Keep expand button here
          label = NULL,
          icon = icon("chevron-right"),
          class = "btn btn-secondary expand-sidebar-btn"
        )
      ),
      # Map Column - will expand when sidebar collapses
      div(
        id = ns("mapCol"),
        class = "col-md-9 map-column",
        map_module_ui(ns("map"))
      )
    )
  )

  # Wrap everything in tagList with styles
  tagList(
    # Include CSS resources
    tags$head(
      # Global styles (with RTBM styles now integrated into main.scss)
      tags$link(rel = "stylesheet", type = "text/css", href = "styles/main.css")
    ),
    useShinyjs(), # Initialize shinyjs
    base_layout,
    # Add plot output for summary statistics
    card(
      card_header("Summary Statistics"),
      card_body(
        radioButtons(
          inputId = ns("summary_plot_choice"),
          label = "Choose Summary View:",
          choices = c(
            "Overall Activity" = "overall",
            "Species Ranking" = "rank",
            "Data Table" = "table"
          ),
          selected = "overall",
          inline = TRUE
        ),
        conditionalPanel(
          condition = paste0("input['", ns("summary_plot_choice"), "'] != 'table'"),
          plotOutput(ns("summary_plot"), height = "600px")
        ),
        conditionalPanel(
          condition = paste0("input['", ns("summary_plot_choice"), "'] == 'table'"),
          DTOutput(ns("summary_table"))
        )
      )
    )
  )
}

#' Real-time Bird Monitoring Server Module
#'
#' @param id The module ID
#' @param tab_selected Reactive expression for tab selection
#'
#' @return A Shiny server function
#' @export
rtbm_app_server <- function(id, tab_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Main App Reactives ---
    available_dates <- reactiveVal(NULL)
    bird_spp_info <- reactiveVal(NULL)
    species_data <- reactiveVal(NULL) # Data per species for the map
    summary_data <- reactiveVal(NULL) # Preloaded summary data
    loaded_earliest_date <- reactiveVal(NULL)
    loaded_latest_date <- reactiveVal(NULL)
    all_summary_data_store <- reactiveVal(tibble(date = as.Date(character(0)))) # Initialize with empty tibble with date column

    # --- Initial Data Loading ---
    # Load Finland border and bird species info only when tab is selected
    observeEvent(tab_selected(), ignoreInit = TRUE, {
      # Load bird species data immediately so the dropdown is populated
      if (is.null(bird_spp_info())) {
        print("Loading bird species info on tab selection...")
        bird_spp_info(load_bird_species_info())
      }
    })

    # --- Sidebar Module Call ---
    sidebar_returns <- rtbm_sidebar_server(
      id = "sidebar",
      bird_spp_info = bird_spp_info,
      available_dates = available_dates
    )

    # --- Connect Sidebar Outputs to App Logic ---

    # React to date range changes from sidebar to update available_dates
    observeEvent(sidebar_returns$date_range(),
      {
        req(sidebar_returns$date_range())
        start_date <- sidebar_returns$date_range()[1]
        end_date <- sidebar_returns$date_range()[2]

        # Find parquet files within the date range (Uses config)
        data_path <- get("data_path") # Get data path from config
        if (is.null(data_path) || !dir_exists(data_path)) {
          warning("Data path not configured or does not exist.")
          available_dates(NULL)
          return()
        }

        parquet_dir <- file.path(data_path, "rtbm", "parquet")
        if (!dir_exists(parquet_dir)) {
          warning(paste("Parquet directory not found:", parquet_dir))
          available_dates(NULL)
          return()
        }

        parquet_files <- list.files(
          path = parquet_dir,
          pattern = "\\.parquet$",
          full.names = TRUE
        )

        if (length(parquet_files) > 0) {
          # Extract dates robustly, handle potential NA
          file_dates_str <- str_extract(basename(parquet_files), "\\d{8}")
          file_dates <- ymd(file_dates_str, quiet = TRUE)
          valid_file_indices <- !is.na(file_dates)

          file_dates <- file_dates[valid_file_indices]

          if (length(file_dates) > 0) {
            valid_indices <- which(file_dates >= start_date & file_dates <= end_date)
            if (length(valid_indices) > 0) {
              dates_in_range <- sort(unique(file_dates[valid_indices]))
              available_dates(dates_in_range) # Update the main app's reactive
            } else {
              available_dates(NULL)
            }
          } else {
            available_dates(NULL)
          }
        } else {
          available_dates(NULL)
        }
      },
      ignoreNULL = FALSE
    ) # Trigger on initial load

    # --- Data Loading Triggered by Button ---
    # Observe the button click from the sidebar
    observeEvent(sidebar_returns$load_button_clicked(), {
      # Get essential inputs using isolate
      selected_view <- isolate(sidebar_returns$selected_view())
      species <- isolate(sidebar_returns$selected_species())
      date_range_val <- isolate(sidebar_returns$date_range())

      # --- Conditional Data Loading based on View Selected ---
      if (selected_view == "map") {
        print("Load Data clicked for Map view")
        # --- Load Map-Specific Data ---
        # Validate species and date range inputs for map view
        req(species, date_range_val)
        start_date <- date_range_val[1]
        end_date <- date_range_val[2]
        if (is.null(species) || species == "" || is.na(species)) {
          output$statusMsg <- renderUI({
            div(
              class = "alert alert-danger",
              role = "alert",
              "Error: Please select a species for the map view."
            )
          })
          return()
        }

        # Find scientific name
        scientific_name_val <- isolate(bird_spp_info()) |>
          filter(common_name == species) |>
          pull(scientific_name)

        if (length(scientific_name_val) == 0) {
          output$statusMsg <- renderUI({
            div(
              class = "alert alert-danger",
              role = "alert",
              "Error: Could not find scientific name for the selected species."
            )
          })
          return()
        }

        # Update status message for loading
        output$statusMsg <- renderUI({
          div(
            class = "alert alert-info",
            role = "alert",
            "Loading map data... This may take a few moments."
          )
        })

        # Load species observation data (parquet files)
        result <- NULL
        withProgress(
          message = "Processing map data",
          value = 0,
          {
            incProgress(0.2, detail = "Fetching observation data")
            result <- load_parquet_data(
              scientific_name = scientific_name_val,
              start_date = start_date,
              end_date = end_date
            )
            incProgress(0.3, detail = "Processing observations")

            # Handle no data scenario
            if (is.null(result$data) || nrow(result$data) == 0 || length(result$dates) == 0) {
              output$statusMsg <- renderUI({
                div(
                  class = "alert alert-warning",
                  role = "alert",
                  "No map data available for the selected species and date range."
                )
              })
              available_dates(NULL)
              species_data(NULL)
              return()
            }

            # Store dates and data paths
            available_dates(result$dates)
            species_data(list(
              scientific_name = scientific_name_val,
              data_paths = result$data_paths
            ))

            # Set the current date in the sidebar
            if (length(result$dates) > 0) {
              sidebar_returns$set_current_date(result$dates[1])
            }

            # Update status message on success
            output$statusMsg <- renderUI({
              div(
                class = "alert alert-success",
                role = "alert",
                paste0("Loaded map data for ", length(result$dates), " dates.")
              )
            })

            # Update map with the first frame
            map_functions$update_map_with_frame()
          }
        )
      } else if (selected_view == "summary") { # UPDATED CONDITION
        print(paste("Load Data clicked for Summary view"))
        # --- Load Summary-Specific Data ---
        req(date_range_val)
        start_date_summary <- date_range_val[1]
        end_date_summary <- date_range_val[2]

        # Update status message for loading
        output$statusMsg <- renderUI({
          div(
            class = "alert alert-info",
            role = "alert",
            "Loading summary data..."
          )
        })

        # Isolate to prevent re-triggering from dependent reactives changing inside
        isolate({
          current_earliest <- loaded_earliest_date()
          current_latest <- loaded_latest_date()
          current_store <- all_summary_data_store()

          # --- Determine date ranges to fetch --- #
          dates_to_fetch_before <- NULL
          dates_to_fetch_after <- NULL # Ensure this is initialized to NULL

          if (is.null(current_earliest)) { # Cache is empty, or first load
            # Fetch the entire requested range if valid
            if (start_date_summary <= end_date_summary) {
              dates_to_fetch_before <- list(start = start_date_summary, end = end_date_summary)
            }
            # dates_to_fetch_after remains NULL, ensuring only one fetch operation
          } else {
            # Cache exists, determine segments to fetch before the current earliest
            if (start_date_summary < current_earliest) {
              # Fetch from selected start_date up to one day before current_earliest,
              # but not exceeding selected end_date.
              fetch_end_b <- min(end_date_summary, current_earliest - days(1))
              if (start_date_summary <= fetch_end_b) {
                dates_to_fetch_before <- list(start = start_date_summary, end = fetch_end_b)
              }
            }

            # Determine segments to fetch after the current latest
            if (end_date_summary > current_latest) {
              # Fetch from one day after current_latest up to selected end_date,
              # but not before selected start_date.
              fetch_start_a <- max(start_date_summary, current_latest + days(1))
              if (fetch_start_a <= end_date_summary) {
                dates_to_fetch_after <- list(start = fetch_start_a, end = end_date_summary)
              }
            }
          }

          data_before <- NULL
          if (!is.null(dates_to_fetch_before)) {
            message(paste("Fetching summary data for range (before):", dates_to_fetch_before$start, "to", dates_to_fetch_before$end))
            data_before <- preload_summary_data(start_date = dates_to_fetch_before$start, end_date = dates_to_fetch_before$end)
          }

          data_after <- NULL
          if (!is.null(dates_to_fetch_after)) {
            message(paste("Fetching summary data for range (after):", dates_to_fetch_after$start, "to", dates_to_fetch_after$end))
            data_after <- preload_summary_data(start_date = dates_to_fetch_after$start, end_date = dates_to_fetch_after$end)
          }

          # --- Combine data --- #
          # Ensure current_store is a tibble, even if it was NULL or not yet set properly.
          if (is.null(current_store) || !is_tibble(current_store)) {
            current_store <- tibble(date = as.Date(character(0)))
          }

          # Prepare list of data frames to bind, removing NULLs
          updated_store_list <- list(current_store, data_before, data_after)
          updated_store_list <- discard(updated_store_list, is.null)
          updated_store_list <- discard(updated_store_list, ~ nrow(.) == 0) # Remove empty tibbles

          if (length(updated_store_list) > 0) {
            updated_store <- bind_rows(updated_store_list)

            # Replace NAs (from new species columns) with 0 for count columns
            cols_to_fill_na <- setdiff(names(updated_store), "date")
            if (length(cols_to_fill_na) > 0) {
              updated_store <- updated_store |>
                mutate(across(all_of(cols_to_fill_na), ~ replace_na(.x, 0)))
            }

            updated_store <- updated_store |>
              distinct(date, .keep_all = TRUE) |>
              arrange(date)
          } else {
            # Fallback if all parts are NULL or empty
            updated_store <- tibble(date = as.Date(character(0)))
          }
          all_summary_data_store(updated_store)

          # --- Update loaded date range tracker --- #
          if (nrow(updated_store) > 0) {
            new_loaded_earliest <- min(updated_store$date, na.rm = TRUE)
            new_loaded_latest <- max(updated_store$date, na.rm = TRUE)
            loaded_earliest_date(new_loaded_earliest)
            loaded_latest_date(new_loaded_latest)
            message(paste("Updated summary store. New loaded range:", new_loaded_earliest, "to", new_loaded_latest, ". Rows:", nrow(updated_store)))
          } else {
            loaded_earliest_date(NULL)
            loaded_latest_date(NULL)
            message("Updated summary store is empty. Resetting loaded range.")
          }

          # --- Filter data for display --- #
          if (nrow(updated_store) > 0) {
            filtered_display_data <- updated_store |>
              filter(date >= start_date_summary & date <= end_date_summary)
            summary_data(filtered_display_data)
            message(paste("Summary data updated for display. Range:", start_date_summary, "to", end_date_summary, ". Rows:", nrow(filtered_display_data)))
          } else {
            summary_data(tibble(date = as.Date(character(0)))) # Ensure consistent empty structure
            message("No summary data to display for the selected range.")
          }
        })

        # Clear status message or set a success message for summary
        output$statusMsg <- renderUI({
          NULL
        }) # Or a success message
      } else {
        # Handle unknown view selection
        print(paste("Load Data clicked for unknown view:", selected_view))
        output$statusMsg <- renderUI({
          div(
            class = "alert alert-warning",
            role = "alert",
            paste("Unknown view selected:", selected_view)
          )
        })
      }
      # --- End Conditional Data Loading ---
    })

    # --- Output: Render Summary Plot ---
    output$summary_plot <- renderPlot({
      req(summary_data(), input$summary_plot_choice %in% c("overall", "rank"))

      print(paste("Rendering summary plot:", input$summary_plot_choice))

      # Conditionally call the correct plotting function
      if (input$summary_plot_choice == "overall") {
        create_summary_plots(summary_data())
      } else if (input$summary_plot_choice == "rank") {
        create_top_species_rank_plot(summary_data())
      }
    })

    # --- Output: Render Summary Table ---
    output$summary_table <- renderDT({
      req(summary_data(), bird_spp_info(), input$summary_plot_choice == "table")

      print("Rendering summary table...")

      table_data <- create_top_species_table_data(summary_data(), bird_spp_info())

      datatable(table_data,
        options = list(pageLength = 10),
        rownames = FALSE,
        colnames = c("Date", "Vernacular name", "Scientific name", "Count")
      )
    })

    # --- Map Module Call ---
    map_functions <- map_module_server(
      "map",
      current_date = sidebar_returns$current_date, # Use reactive from sidebar
      species_data = species_data, # Pass the processed species data
      selected_species = sidebar_returns$selected_species, # Use reactive from sidebar
      bird_spp_info = bird_spp_info # Pass all species info
    )
  })
}

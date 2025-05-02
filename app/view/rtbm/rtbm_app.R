# External packages
box::use(
  # Shiny fundamentals and UI
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
    insertUI,
    uiOutput,
    renderText,
    updateDateRangeInput,
    updateSelectInput,
    eventReactive,
    icon,
    textInput,
    showNotification,
    verbatimTextOutput,
    renderPrint,
    textOutput
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
    card_footer,
    tooltip,
    value_box,
    layout_column_wrap,
    page_fluid,
    nav_panel,
    nav_spacer,
    nav_menu,
    nav_item,
    nav,
    navset_bar,
    as_fill_carrier,
    as_fill_item
  ],

  # Modern Shiny components
  shinyjs[useShinyjs, runjs, hide, show, toggle, addClass, removeClass, toggleClass, delay],

  # Data manipulation with tidyverse
  dplyr[filter, mutate, select, pull, arrange, group_by, summarize, n, between, row_number, slice],

  # Date and time handling
  lubridate[as_date, ymd, interval, `%within%`],
  tibble[tibble, as_tibble],
  stringr[str_replace],
  tidyr[unnest, pivot_longer],

  # File handling with arrow (Apache Arrow)
  arrow[read_parquet, write_parquet, Schema, schema],

  # Spatial data handling
  sf[st_read, st_drop_geometry, st_as_sf, st_bbox, st_coordinates, st_as_sfc, st_crs],

  # Basic utilities
  utils[head, tail],
  jsonlite[fromJSON, toJSON],
  fs[dir_exists],
)

# Local modules
box::use(
  app / logic / rtbm / rtbm_data_handlers[load_bird_species_info, load_parquet_data, get_finland_border, preload_summary_data],
  app / view / rtbm / rtbm_map[map_module_ui, map_module_server],
  app / view / rtbm / rtbm_sidebar[rtbm_sidebar_ui, rtbm_sidebar_server],
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
    base_layout
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
    finland_border <- reactiveVal(NULL)
    available_dates <- reactiveVal(NULL)
    bird_spp_info <- reactiveVal(NULL)
    species_data <- reactiveVal(NULL) # Data per species for the map
    summary_data <- reactiveVal(NULL) # Preloaded summary data

    # --- Initial Data Loading ---
    # Load Finland border and bird species info only when tab is selected
    observeEvent(tab_selected(), ignoreInit = TRUE, {
      # Load bird species data immediately so the dropdown is populated
      if (is.null(bird_spp_info())) {
        print("Loading bird species info on tab selection...")
        bird_spp_info(load_bird_species_info())
      }
      # Other initial loads (border, summary) are deferred to button click
      print("RTBM Tab selected.")
    })

    # --- Sidebar Module Call ---
    sidebar_returns <- rtbm_sidebar_server(
      id = "sidebar",
      bird_spp_info = bird_spp_info, # Pass bird info IN
      available_dates = available_dates # Pass available dates IN
    )

    # --- Connect Sidebar Outputs to App Logic ---

    # React to date range changes from sidebar to update available_dates
    observeEvent(sidebar_returns$date_range(),
      {
        req(sidebar_returns$date_range())
        start_date <- sidebar_returns$date_range()[1]
        end_date <- sidebar_returns$date_range()[2]

        # Find parquet files within the date range (Uses config)
        data_path <- config::get("data_path") # Get data path from config
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
          file_dates_str <- stringr::str_extract(basename(parquet_files), "\\d{8}")
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

      # --- Conditional Data Loading based on View ---
      if (selected_view == "map") {
        print("Load Data clicked for Map view")
        # --- Load Map-Specific Data ---
        # Load Finland border (only on first button click for map)
        isolate({
          if (is.null(finland_border())) {
            print("Loading Finland border on button click...")
            finland_border(get_finland_border())
          }
        })

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

            # Check if species directory exists (redundant check, load_parquet_data should handle)
            # ... (Consider removing if load_parquet_data is robust)

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
      } else if (selected_view %in% c("fig3", "fig4", "fig5")) {
        print(paste("Load Data clicked for Summary view:", selected_view))
        # --- Load Summary-Specific Data ---
        req(date_range_val) # Assuming summary might need date range
        start_date <- date_range_val[1]
        end_date <- date_range_val[2]

        # Update status message for loading
        output$statusMsg <- renderUI({
          div(
            class = "alert alert-info",
            role = "alert",
            "Loading summary data..."
          )
        })

        # Load summary data if not already loaded
        # Pass date range if preload_summary_data uses it (modify function if needed)
        isolate({
          if (is.null(summary_data())) {
            print("Preloading summary data on button click...")
            summary_data(preload_summary_data(start_date = start_date, end_date = end_date))

            if (!is.null(summary_data())) {
              print(paste("Summary data preloaded successfully. Rows:", nrow(summary_data())))
              output$statusMsg <- renderUI({
                div(
                  class = "alert alert-success",
                  role = "alert",
                  "Summary data loaded successfully."
                )
              })
            } else {
              print("Failed to preload summary data.")
              output$statusMsg <- renderUI({
                div(
                  class = "alert alert-danger",
                  role = "alert",
                  "Failed to load summary data."
                )
              })
            }
          } else {
            # Data already loaded, update status
            print("Summary data already loaded.")
            output$statusMsg <- renderUI({
              div(
                class = "alert alert-success",
                role = "alert",
                "Summary data already loaded."
              )
            })
          }
        })
        # TODO: Add logic here to render the specific summary plot (fig3/4/5) based on selected_view
        # This might involve calling another module or rendering logic within this module.
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

    # --- Map Module Call ---
    map_functions <- map_module_server(
      "map",
      finland_border = finland_border,
      current_date = sidebar_returns$current_date, # Use reactive from sidebar
      species_data = species_data, # Pass the processed species data
      selected_species = sidebar_returns$selected_species, # Use reactive from sidebar
      bird_spp_info = bird_spp_info # Pass all species info
    )
  })
}

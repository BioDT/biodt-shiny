# Real-Time Bird Monitoring (RTBM) Module

## Overview

The Real-Time Bird Monitoring (RTBM) module provides interactive visualization of bird species distribution data in Finland. This user-friendly tool helps researchers track and analyze bird populations with intuitive controls and informative visualizations.

This document provides an overview of the Shiny modules responsible for the Real-Time Bird Monitoring (RTBM) interface.

## 1. RTBM Application Module (`app/view/rtbm/rtbm_app.R`)

This module serves as the main container for the RTBM visualization tool. It orchestrates the user interface layout, data loading controls, timeline animation, and communication with the map module.

### Exported Functions

*   `rtbm_app_ui(id, i18n)`: Creates the user interface for the RTBM tab. This includes the control panel sidebar (date range, species picker, load button, timeline slider) and the main map display area.
*   `rtbm_app_server(id, tab_selected)`: Defines the server-side logic for the RTBM tab. It handles user inputs, manages application state (available dates, current date, species data, animation state), processes data loading triggers, and initializes the map module.

### Reactive Values & Observers

*   Manages application state using `reactiveVal`: `finland_border`, `available_dates`, `bird_spp_info`, `species_data`, `summary_data`, `loaded_earliest_date`, `loaded_latest_date`, `all_summary_data_store`. (Note: some previous reactiveVals like `current_date`, `current_frame_index`, `animation_running` are now managed within the sidebar module or handled differently).
*   Uses `reactive` expressions to derive values based on inputs.
*   Uses `observeEvent` to handle user actions (load data button, timeline slider changes, animation controls). It also uses `observeEvent(tab_selected(), ...)` to conditionally load `bird_spp_info()` and `finland_border()` when the RTBM tab becomes active.
*   Uses `eventReactive` for certain data processing triggers.
*   Includes observers for animation, potentially managed within sub-modules like the sidebar.

### Dependencies on `app/logic/rtbm/`

*   **From `rtbm_data_handlers`:**
    *   `load_bird_species_info()`: Called within an `observeEvent(tab_selected(), ...)` to populate the species picker dropdown when the tab is selected.
    *   `load_finland_border_geojson()`: Called within an `observeEvent(tab_selected(), ...)` to load the Finland border vector data when the tab is selected.
    *   `load_parquet_data(scientific_name, start_date, end_date)`: Called when the "Load Data" button is clicked (for the map view) to fetch observation data.
    *   `preload_summary_data(start_date, end_date)`: Called when the "Load Data" button is clicked (for the summary view) to fetch aggregated summary data.

### Dependencies on other Local Modules

*   **From `app/view/rtbm/rtbm_sidebar`:**
    *   `rtbm_sidebar_ui(ns("sidebar"))`: Called within `rtbm_app_ui` to embed the sidebar UI component.
    *   `rtbm_sidebar_server(...)`: Called within `rtbm_app_server` to initialize the sidebar module's server logic and retrieve its reactive outputs.
*   **From `app/view/rtbm/rtbm_map`:**
    *   `map_module_ui(ns("map"))`: Called within `rtbm_app_ui` to embed the map UI component.
    *   `map_module_server(...)`: Called within `rtbm_app_server` to initialize the map module's server logic, passing necessary reactive values and data (including the reactive `finland_border`, `current_date` from sidebar, `species_data`, and `bird_spp_info`).

---

## 2. RTBM Map Module (`app/view/rtbm/rtbm_map.R`)

This module is dedicated to displaying the Leaflet map, rendering bird observation heatmaps, handling map controls (legend, date display), and showing the bird information card.

### Exported Functions

*   `map_module_ui(id)`: Creates the UI element for the Leaflet map output.
*   `map_module_server(id, finland_border, current_date, species_data, selected_species, bird_spp_info)`: Defines the server logic for the map. It receives reactive data from the parent module (`rtbm_app.R`), including `finland_border` (a reactive value holding the Finland border sf object), and is responsible for rendering the base map, updating layers, adding controls, and managing the bird info card. (Note: `photo_url`, `scientific_name`, `wiki_link`, `song_url` are now derived internally or via `bird_spp_info`). Returns a list containing the `update_map_with_frame` function.

### Internal Helper Functions

*   `create_bird_info_card(...)`: Creates the HTML structure for the bird information card using `htmltools`.
*   `update_bird_info_card()`: Adds, updates, or removes the bird info card control on the map using `leafletProxy` based on the reactive inputs.
*   `update_map_with_frame()`: The core function for updating the map display. It reads the appropriate parquet file based on the `current_date()` and `species_data()`, validates the data, clears existing layers/controls using `leafletProxy`, and adds the Finland border, observation heatmap, legend, and date display to the map.
*   `create_hover_info_display()`: Helper to create the (currently unused) hover info div structure.
*   `create_date_display(date_str)`: Helper to create the date display control HTML.
*   `create_error_display(error_message)`: Helper to create an error message control HTML.
*   `create_status_message(message, type)`: Helper to create status message controls (currently unused in this module).
*   `safe_print(...)`: Internal helper for controlled console logging.

### Reactive Values & Observers

*   Manages internal state `info_card_visible` using `reactiveVal`.
*   Uses `renderLeaflet` (`output$rasterMap`) to create the initial base map.
*   Uses `observeEvent` to react to changes in the bird information reactives passed from the parent (`selected_species()`, `photo_url()`, etc.) and trigger `update_bird_info_card()`.

### Dependencies on `app/logic/rtbm/`

*   **From `rtbm_data_handlers`:**
    *   `load_bird_species_info()`: Currently imported but **not used** within `rtbm_map.R` itself (its result is used in `rtbm_app.R` and relevant derived values are passed *into* `map_module_server`). This import could potentially be removed from `rtbm_map.R`.

---

## 3. RTBM Sidebar Module (`app/view/rtbm/rtbm_sidebar.R`)

This module manages the user interface and server-side logic for the control sidebar within the RTBM application. It provides controls for view selection, date range, species picking, data loading triggers, and map animation.

### Exported Functions

*   `rtbm_sidebar_ui(id)`: Creates the sidebar's UI. This includes:
    *   A view selector (Map/Summary).
    *   Date range input.
    *   A bird species picker (`shinyWidgets::pickerInput`), which is dynamically populated and shown/hidden based on the selected view.
    *   A "Load Data" button.
    *   A conditional UI section (`uiOutput(ns("conditionalMapControls"))`) that displays map-specific animation controls (timeline slider, play/pause button, animation speed slider) only when the "Map" view is active and data is available.
*   `rtbm_sidebar_server(id, bird_spp_info, available_dates)`: Defines the server logic for the sidebar.
    *   **Inputs**:
        *   `bird_spp_info`: A reactive value containing the list of bird species for the picker.
        *   `available_dates`: A reactive value containing the observation dates available for the selected date range, used to populate the timeline slider.
    *   **Returns a list of reactive values/expressions for the parent module (`rtbm_app.R`)**:
        *   `current_date`: The currently selected date from the timeline slider.
        *   `set_current_date`: A reactiveVal setter for `current_date`.
        *   `selected_species`: The species selected in the picker.
        *   `date_range`: The selected date range from the `dateRangeInput`.
        *   `animation_running`: A boolean indicating if the map animation is currently playing.
        *   `selected_view`: The view selected ("map" or "summary").
        *   `load_button_clicked`: A reactive trigger that fires when the "Load Data" button is clicked.

### Internal Reactive Values & Observers

*   Manages internal state using `reactiveVal` for `current_date_rv`, `animation_running_rv`, `animation_speed_rv`.
*   Uses `observeEvent` to handle:
    *   Updates to `bird_spp_info` (to repopulate the species picker).
    *   Updates to `available_dates` (to update the timeline slider).
    *   Changes to animation controls (play/pause button `input$animateControl`, speed slider `input$speedControl`).
*   Includes an `animation_step()` internal function to advance the timeline slider during animation, driven by `invalidateLater`.
*   Dynamically renders UI components like the timeline slider, play/pause button, selected date display, and status messages based on application state.

### Dependencies on `app/logic/rtbm/`

*   **From `app/logic/rtbm/utils`:**
    *   `format_date_for_display()`: Used to format dates for display (e.g., selected date below the timeline slider).

---

## External Links
- Bird photos API (https://bird-photos.a3s.fi/)
- Distribution data API (https://2007581-webportal.a3s.fi/)

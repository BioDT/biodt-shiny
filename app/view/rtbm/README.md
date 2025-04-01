# Real-Time Bird Monitoring (RTBM) Module

## Overview

The Real-Time Bird Monitoring (RTBM) module provides interactive visualization of bird species distribution data in Finland. This user-friendly tool helps researchers track and analyze bird populations with intuitive controls and informative visualizations.

This document provides an overview of the Shiny modules responsible for the Real-Time Bird Monitoring (RTBM) interface.

## 1. RTBM Application Module (`app/view/rtbm/rtbm_app.R`)

This module serves as the main container for the RTBM visualization tool. It orchestrates the user interface layout, data loading controls, timeline animation, and communication with the map module.

### Exported Functions

*   `rtbm_app_ui(id, i18n)`: Creates the user interface for the RTBM tab. This includes the control panel sidebar (date range, species picker, load button, timeline slider) and the main map display area.
*   `rtbm_app_server(id, tab_selected)`: Defines the server-side logic for the RTBM tab. It handles user inputs, manages application state (available dates, current date, species data, animation state), processes data loading triggers, and initializes the map module.

### Internal Helper Functions

*   `get_finland_border()`: Loads the GeoJSON file defining the border of Finland for map display. Includes fallback to a bounding box.
*   `create_finland_bbox()`: Creates a simple bounding box for Finland as a fallback if the GeoJSON file cannot be loaded.
*   `format_date_for_display(date)`: Formats a date object into a user-friendly string (e.g., "Mar 15, 2025").
*   `format_dates_for_display(dates)`: Applies `format_date_for_display` to a vector of dates.
*   `process_all_dates()`: Orchestrates the data loading process for the selected species and date range by calling `load_parquet_data`. Handles progress display and status messages. Updates reactive values (`available_dates`, `species_data`, `current_date`). Triggers the initial map update.
*   `animation_step()`: Advances the timeline slider and map display by one step during animation.

### Reactive Values & Observers

*   Manages application state using `reactiveVal`: `available_dates`, `current_date`, `current_frame_index`, `species_data`, `animation_running`, `animation_last_step`, `sidebar_expanded`, `legend_added`, `info_card_added`.
*   Uses `reactive` expressions to derive values based on inputs (e.g., `finnish_name`, `photo_url`, `scientific_name`, `wiki_link`, `song_url`, `date_sequence`).
*   Uses `observeEvent` to handle user actions (sidebar toggling, load data button, timeline slider changes, animation controls).
*   Uses `eventReactive` (`raster_data`) to trigger data processing only when the "Load Data" button is clicked.
*   Includes observers for sidebar state management (`observeEvent(input$toggleSidebar, ...)` etc.) and animation (`observe({...})`).

### Dependencies on `app/logic/rtbm/`

*   **From `rtbm_data_handlers`:**
    *   `load_bird_species_info()`: Used at the start to populate the species picker dropdown.
    *   `load_parquet_data(scientific_name, start_date, end_date)`: Called by `process_all_dates` when the "Load Data" button is clicked to fetch and potentially process observation data for the selected species and date range.

### Dependencies on other Local Modules

*   **From `app/view/rtbm/rtbm_map`:**
    *   `map_module_ui(ns("map"))`: Called within `rtbm_app_ui` to embed the map UI component.
    *   `map_module_server(...)`: Called within `rtbm_app_server` to initialize the map module's server logic, passing necessary reactive values and data (border, date, species data, species info URLs).

---

## 2. RTBM Map Module (`app/view/rtbm/rtbm_map.R`)

This module is dedicated to displaying the Leaflet map, rendering bird observation heatmaps, handling map controls (legend, date display), and showing the bird information card.

### Exported Functions

*   `map_module_ui(id)`: Creates the UI element for the Leaflet map output.
*   `map_module_server(id, finland_border, current_date, species_data, selected_species, photo_url, scientific_name, wiki_link, song_url)`: Defines the server logic for the map. It receives reactive data from the parent module (`rtbm_app.R`) and is responsible for rendering the base map, updating layers (border, heatmap), adding controls (legend, date display), and managing the bird info card display. Returns a list containing the `update_map_with_frame` function.

### Internal Helper Functions

*   `create_bird_info_card(...)`: Creates the HTML structure for the bird information card using `htmltools`.
*   `update_bird_info_card()`: Adds, updates, or removes the bird info card control on the map using `leafletProxy` based on the reactive inputs.
*   `update_map_with_frame(frame_index)`: The core function for updating the map display. It reads the appropriate parquet file based on the `current_date()` and `species_data()`, validates the data, clears existing layers/controls using `leafletProxy`, and adds the Finland border, observation heatmap, legend, and date display to the map.
*   `format_date_for_display(date)`: Formats a date object into a user-friendly string (duplicates the one in `rtbm_app.R`, could potentially be moved to a shared utility module).
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
## External Links
- Bird photos API (https://bird-photos.a3s.fi/)
- Distribution data API (https://2007581-webportal.a3s.fi/)

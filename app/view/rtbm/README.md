# Real-Time Bird Monitoring (RTBM) Module

## Overview

The Real-Time Bird Monitoring (RTBM) module provides interactive visualization of bird species distribution data in Finland. This user-friendly tool helps researchers track and analyze bird populations with intuitive controls and informative visualizations.

The module features two main views:
- **Map View**: Interactive geographic visualization with heatmaps and species-specific information
- **Summary View**: Statistical analysis with activity summaries, species ranking trends, and detailed data tables

This document provides an overview of the Shiny modules responsible for the Real-Time Bird Monitoring (RTBM) interface.

## Module Structure

The RTBM module consists of the following key components:

### Main Application (`rtbm_main.R`)
- **Main navigation**: Three-tab interface (Info, RTBM App, Contributors)
- **Tab management**: Reactive tab selection and coordination

### Application Logic (`rtbm_app.R`)
- **Core application**: Dual-view functionality (Map/Summary)
- **Data orchestration**: Manages data loading, caching, and state
- **Module coordination**: Integrates sidebar, map, and summary components

### User Interface Components
- **Sidebar (`rtbm_sidebar.R`)**: Controls panel with view selection, date range, species picker, and progress indicators
- **Map (`rtbm_map.R`)**: Interactive Leaflet map with heatmaps and bird information cards
- **Info (`rtbm_info.R`)**: Project information and description
- **Contributors (`rtbm_contributors.R`)**: Author attribution and affiliations

---

## 1. RTBM Application Module (`app/view/rtbm/rtbm_app.R`)

This module serves as the main container for the RTBM visualization tool. It orchestrates the dual-view interface, data loading controls, and communication between components.

### Exported Functions

*   `rtbm_app_ui(id, i18n)`: Creates the user interface for the RTBM tab. This includes:
    *   Control panel sidebar with view selector (Map/Summary)
    *   Date range input and progress indicators
    *   Species picker (conditional for map view)
    *   Main display area (map or summary statistics)
    *   Summary statistics card with plot selection controls

*   `rtbm_app_server(id, tab_selected)`: Defines the server-side logic for the RTBM tab. It handles:
    *   User inputs and view selection
    *   Data loading with progress tracking
    *   Application state management
    *   Module initialization and coordination

### Reactive Values & State Management

*   **Core State**: `finland_border`, `available_dates`, `bird_spp_info`, `species_data`, `summary_data`
*   **Data Caching**: `loaded_earliest_date`, `loaded_latest_date`, `all_summary_data_store`
*   **Progress Tracking**: `summary_progress_info` for real-time loading feedback
*   **View Management**: Conditional data loading based on selected view (Map/Summary)

### Dual-View Data Loading

The application implements intelligent data loading based on the selected view:

#### Map View
- Species-specific observation data loading
- Parquet file processing for heatmap visualization
- Date-based frame navigation
- Progress tracking with leaflet integration

#### Summary View
- Aggregated multi-species data loading
- Incremental cache management (before/after date ranges)
- Progress callback system with sidebar integration
- Optimized data fetching to minimize redundant requests

### Summary Statistics Features

*   **Activity Summary**: Cumulative records, daily counts, and species diversity metrics
*   **Top 5 Species Rank Trends**: Dynamic ranking visualization with common names
*   **Top 5 Daily Species Counts**: Detailed observation data in interactive tables

### Dependencies on `app/logic/rtbm/`

*   **From `rtbm_data_handlers`:**
    *   `load_bird_species_info()`: Populates species picker dropdown
    *   `load_finland_border_geojson()`: Loads Finland border vector data
    *   `load_parquet_data(scientific_name, start_date, end_date)`: Fetches map observation data
    *   `preload_summary_data(start_date, end_date, progress_callback)`: Fetches aggregated summary data with progress tracking

*   **From `rtbm_summary_plots`:**
    *   `create_summary_plots(data)`: Generates activity summary visualizations
    *   `create_top_species_rank_plot(data, bird_spp_info)`: Creates species ranking trends
    *   `create_top_species_table_data(data, bird_spp_info)`: Prepares table data with common names

### Dependencies on other Local Modules

*   **From `app/view/rtbm/rtbm_sidebar`:**
    *   `rtbm_sidebar_ui(ns("sidebar"))`: Embeds the sidebar UI component
    *   `rtbm_sidebar_server(...)`: Initializes sidebar logic and retrieves reactive outputs

*   **From `app/view/rtbm/rtbm_map`:**
    *   `map_module_ui(ns("map"))`: Embeds the map UI component
    *   `map_module_server(...)`: Initializes map logic with reactive data streams

---

## 2. RTBM Map Module (`app/view/rtbm/rtbm_map.R`)

This module displays the interactive Leaflet map with bird observation heatmaps, legends, and species information cards.

### Exported Functions

*   `map_module_ui(id)`: Creates the Leaflet map output element
*   `map_module_server(id, finland_border, current_date, species_data, selected_species, bird_spp_info)`: Defines the map server logic with real-time updates

### Key Features

#### Heatmap Visualization
- **Color Scheme**: YlGnBu palette (light yellow to dark blue) for intensity mapping
- **Dynamic Scaling**: Automatic intensity scaling based on observation density
- **Legend Integration**: Consistent color mapping between heatmap and legend

#### Bird Information Cards
- **Species Details**: Common name, scientific name (italicized), and Wikipedia links
- **Visual Content**: Species photos from external API
- **Audio Integration**: Bird song playback functionality
- **Dynamic Updates**: Real-time card updates based on species selection

#### Map Controls
- **Date Display**: Current observation date in bottom-left corner
- **Progress Indicators**: Real-time loading feedback in bottom-right
- **Error Handling**: Graceful error display for data loading issues
- **Debug Information**: Coordinate ranges and data point counts (in development mode)

### Internal Helper Functions

*   `create_bird_info_card(...)`: Builds HTML structure for species information display
*   `update_bird_info_card()`: Manages card visibility and content updates
*   `update_map_with_frame()`: Core function for map layer updates and data visualization
*   `create_date_display(date_str)`: Formats date display control
*   `create_error_display(error_message)`: Handles error message presentation
*   `safe_print(...)`: Controlled console logging for debugging

### Reactive Values & Observers

*   **State Management**: `info_card_visible`, `photo_url`, `wiki_link`, `scientific_name`, `song_url`
*   **Map Rendering**: `renderLeaflet` for base map initialization
*   **Dynamic Updates**: `observeEvent` for species selection and date changes
*   **Proxy Operations**: `leafletProxy` for efficient layer updates without full re-rendering

### Data Processing Pipeline

1. **Parquet File Reading**: Direct arrow-based data loading
2. **Data Validation**: Coordinate range validation and intensity checks
3. **Heatmap Generation**: leaflet.extras integration with custom scaling
4. **Legend Creation**: Synchronized color mapping with observation intensity
5. **Layer Management**: Efficient clearing and updating of map elements

---

## 3. RTBM Sidebar Module (`app/view/rtbm/rtbm_sidebar.R`)

This module manages the control interface, providing view selection, data loading controls, progress tracking, and animation features.

### Exported Functions

*   `rtbm_sidebar_ui(id)`: Creates the comprehensive sidebar interface including:
    *   **View Selector**: Toggle between Map and Summary views
    *   **Date Range Input**: Configurable date selection with validation
    *   **Progress Indicators**: Real-time loading feedback for summary view
    *   **Species Picker**: Searchable dropdown (conditional for map view)
    *   **Load Data Button**: Triggers data fetching operations
    *   **Animation Controls**: Timeline slider, play/pause, and speed controls (map view only)

*   `rtbm_sidebar_server(id, bird_spp_info, available_dates, summary_progress_info)`: Comprehensive server logic handling:
    *   **Input Processing**: View selection, date range validation, species selection
    *   **Animation Management**: Timeline control, play/pause state, speed adjustment
    *   **Progress Tracking**: Real-time feedback integration with data loading
    *   **UI State Management**: Conditional display based on view and data availability

### Enhanced Features

#### Progress Indicators
- **Summary View Integration**: Real-time progress display during data loading
- **Visual Feedback**: Animated progress bars with completion percentages
- **Message Display**: Detailed status messages (e.g., "Fetching 5 of 30: 2025-05-01")
- **Automatic Clearing**: Progress indicators disappear when loading completes

#### Animation System
- **Timeline Navigation**: Slider-based date selection with smooth transitions
- **Play/Pause Control**: Toggle animation with visual state indicators
- **Speed Control**: Adjustable animation delay (100-2000ms)
- **Frame Management**: Automatic looping and frame advancement

#### Conditional UI
- **View-Specific Controls**: Map controls hidden in summary view and vice versa
- **Dynamic Species Picker**: Shown only for map view, hidden for summary analysis
- **Status-Dependent Display**: Controls appear only when relevant data is loaded

### Internal Reactive Values & Observers

*   **Animation State**: `current_date_rv`, `animation_running_rv`, `animation_speed_rv`
*   **UI Management**: Dynamic rendering of date sliders, buttons, and status messages
*   **Event Handling**: Species picker updates, date range changes, animation controls
*   **Progress Integration**: Real-time updates from `summary_progress_info` reactive

### Return Values

Returns a comprehensive list of reactive expressions for parent module integration:
*   `current_date`: Currently selected observation date
*   `set_current_date`: Setter function for programmatic date updates
*   `selected_species`: Currently selected bird species (common name)
*   `date_range`: Selected date range (start and end dates)
*   `animation_running`: Boolean animation state indicator
*   `selected_view`: Current view selection ("map" or "summary")
*   `load_button_clicked`: Reactive trigger for data loading operations

### Dependencies on `app/logic/rtbm/`

*   **From `app/logic/rtbm/utils`:**
    *   `format_date_for_display()`: Consistent date formatting across the interface

---

## 4. Additional Modules

### RTBM Info Module (`app/view/rtbm/rtbm_info.R`)
- **Project Description**: Comprehensive overview of the RTBM prototype Digital Twin
- **Data Sources**: Links to BioDT project and external resources
- **Open Science**: GitHub repository and documentation links
- **Navigation Integration**: Automatic tab switching to main application

### RTBM Contributors Module (`app/view/rtbm/rtbm_contributors.R`)
- **Author Attribution**: Alphabetically sorted list of project contributors
- **Institutional Affiliations**: University and research institute associations
- **International Collaboration**: Multi-country research team representation

### RTBM Main Module (`app/view/rtbm/rtbm_main.R`)
- **Navigation Framework**: Three-tab interface coordination
- **Tab State Management**: Reactive tab selection and event handling
- **Module Integration**: Coordination between info, app, and contributor components

---

## Data Processing & Performance

### Caching Strategy
- **Incremental Loading**: Only fetch missing date ranges to minimize API calls
- **Memory Management**: Efficient storage of aggregated summary data
- **Cache Invalidation**: Automatic updates when date ranges change

### Progress Tracking
- **Real-time Feedback**: Live progress indicators during data fetching
- **User Experience**: Visual confirmation of loading operations
- **Error Handling**: Graceful degradation when data is unavailable

### Visualization Optimization
- **Color Consistency**: Synchronized color palettes between heatmaps and legends
- **Dynamic Scaling**: Automatic adjustment based on data intensity ranges
- **Responsive Design**: Adaptive layout for different screen sizes

---

## External Dependencies & APIs

### Data Sources
- **Bird Photos API**: https://bird-photos.a3s.fi/ (species imagery)
- **Distribution Data API**: https://2007581-webportal.a3s.fi/ (observation data)
- **Wikipedia Integration**: Dynamic links for species information

### Technical Stack
- **Mapping**: Leaflet with heatmap extensions
- **Data Processing**: Apache Arrow (Parquet format)
- **UI Framework**: Shiny with bslib and shinyWidgets
- **Spatial Data**: sf package for geospatial operations

---

## Configuration & Deployment

### Data Path Configuration
- Configurable data directory through `config.yml`
- Support for both local and remote data sources
- Automatic fallback for missing data files

### Environment Requirements
- R environment with tidyverse ecosystem
- Arrow/Parquet support for efficient data processing
- Leaflet and geospatial libraries for mapping functionality

---

## Future Enhancements

### Planned Features
- Additional species information integration
- Enhanced filtering and search capabilities
- Export functionality for visualizations and data
- Mobile-responsive design improvements

### Performance Optimization
- Lazy loading for large datasets
- Client-side caching strategies
- Optimized rendering for high-density observations

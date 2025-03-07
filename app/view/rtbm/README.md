# Real-Time Bird Monitoring (RTBM) Module

## Overview

The Real-Time Bird Monitoring (RTBM) module provides interactive visualization of bird species distribution data in Finland. This module was integrated from a standalone Shiny application into the BioDT framework, enhancing its maintainability, extensibility, and visual consistency with the broader BioDT ecosystem.

## Key Features

- Interactive date selection for historical bird distribution data
- Species selection with searchable dropdown
- Dynamic map visualization of species density across Finland
- Species information cards with photos and taxonomic data
- Integration with external APIs for bird data retrieval

## Module Structure

The RTBM module follows BioDT's modular architecture:

```
app/view/rtbm/
├── rtbm_app.R        # Main module with UI and server components
├── rtbm_contributors.R  # Module contributor information
├── rtbm_info.R       # Info tab content
├── rtbm_main.R       # Main module entry point
├── styles.css        # Module-specific styles
└── README.md         # This documentation
```

## Technical Implementation

### Migration Details

This module was migrated from the standalone application (`biodt-rtbm-webapp/app.R`) to an integrated module within the BioDT framework (`biodt-shiny/app/view/rtbm/`). Key migration aspects include:

1. **Modularization**: Converted monolithic app to Shiny module pattern with proper namespace handling
2. **UI Refactoring**: Replaced direct HTML generation with htmltools for cleaner, more maintainable structure
3. **CSS Consolidation**: Moved inline styles to external CSS file for better separation of concerns
4. **Component Separation**: Split functionality into logical components (main, info, contributors)
5. **Reactive Performance**: Optimized reactive dependencies for better performance
6. **Accessibility**: Added ARIA attributes and improved semantic HTML structure

### Code Refactoring Highlights

#### HTML Generation

- Original: Direct HTML generation using Shiny UI functions
- Refactored: Structured HTML generation using htmltools with proper composition
- Benefits: Clearer structure, better maintainability, easier styling

#### CSS Management

- Original: Inline styles mixed with HTML in app.R
- Refactored: External styles.css file with proper CSS organization
- Specific Migrations:
  - Control panel padding moved to `.control-panel` class
  - Info card photo width moved to `.info-card-photo img` selector
  - Layout and component styles properly organized

#### API Integration

- Preserved the original API connections while improving error handling
- Enhanced the structure of reactive data flows
- Added proper validation for API responses

## Usage

The RTBM module integrates with the main BioDT application and is available as a selectable module from the main navigation. To use the module:

1. Navigate to the RTBM section in the BioDT application
2. Select a date from the date picker
3. Choose a bird species from the dropdown
4. View the distribution map and species information

## Dependencies

- Required R packages: leaflet, terra, httr2, jsonlite, tidyjson, lubridate
- External APIs: 
  - Bird photos API (https://bird-photos.a3s.fi/)
  - Distribution data API (https://2007581-webportal.a3s.fi/)

## Future Enhancements

- Implement caching mechanism for improved performance
- Add time-series visualization of bird movement patterns
- Enhance mobile responsiveness for field use

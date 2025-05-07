# External packages
box::use(
  # Shiny fundamentals and UI
  shiny[NS, moduleServer, reactiveVal, observeEvent, req],

  # HTML tools for structured UI building
  htmltools[
    div, h5, p, em, a, img, tags, css, span, strong
  ],

  # Interactive map
  leaflet[
    leaflet, addTiles, setView, addProviderTiles, clearShapes, addCircles,
    addCircleMarkers, clearControls, addLegend, addPolylines,
    layersControlOptions, addLayersControl, clearImages, colorNumeric, addMarkers,
    clearGroup, makeIcon, addRectangles, removeTiles, hideGroup, showGroup, labelFormat,
    renderLeaflet, leafletOutput, leafletProxy, removeControl, addControl
  ],
  leaflet.extras[addHeatmap],

  # File handling with arrow (Apache Arrow)
  arrow[read_parquet, write_parquet, Schema, schema],

  # Data manipulation with tidyverse
  dplyr[filter, pull, slice],

  # Color palettes
  RColorBrewer[brewer.pal],
  grDevices[colorRampPalette],

  # Spatial data handling
  sf[st_read, st_drop_geometry, st_as_sf, st_bbox, st_coordinates, st_as_sfc, st_crs],
)

# Local modules
box::use(
  app / logic / rtbm / rtbm_data_handlers[load_bird_species_info],
  app / logic / rtbm / utils[format_date_for_display],
)

#' Map Module UI
#'
#' @param id The module ID
#' @return A Shiny UI definition
#' @export
map_module_ui <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("rasterMap"), height = "800px")
}

#' Map Module Server
#'
#' @param id The module ID
#' @param current_date A reactive expression for the current date being displayed
#' @param species_data A reactive expression for the species data
#' @param selected_species A reactive expression for the selected species name
#' @param bird_spp_info A reactive expression containing info for all bird species
#' @return A list containing the update_map_with_frame function
#' @export
map_module_server <- function(id, current_date, species_data,
                              selected_species, bird_spp_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Internal state for the map module
    info_card_visible <- reactiveVal(FALSE)
    photo_url <- reactiveVal(NULL)
    wiki_link <- reactiveVal(NULL)
    scientific_name <- reactiveVal(NULL)
    song_url <- reactiveVal(NULL)

    # Function to create bird info card HTML using htmltools
    create_bird_info_card <- function(species_name, scientific_name_val, wiki_url_val, photo_url_val, song_url_val) {
      # Check for NULLs before creating
      if (is.null(species_name) || is.null(scientific_name_val) || is.null(wiki_url_val) || is.null(photo_url_val) || is.null(song_url_val)) {
        return(NULL)
      }

      div(
        class = "bird-info-card",
        style = css(
          backgroundColor = "white",
          padding = "10px",
          borderRadius = "4px",
          border = "1px solid #dee2e6",
          width = "220px",
          boxShadow = "0 0.125rem 0.25rem rgba(0, 0, 0, 0.075)"
        ),

        # Species name header
        h5(species_name),

        # Scientific name with wiki link
        p(
          em(
            a(
              href = wiki_url_val,
              target = "_blank",
              scientific_name_val
            )
          )
        ),

        # Image container
        div(
          style = css(textAlign = "center"),
          img(
            src = photo_url_val,
            alt = species_name,
            style = css(width = "200px")
          )
        ),

        # Audio player
        div(
          style = css(marginTop = "10px"),
          tags$audio(
            controls = TRUE,
            style = css(width = "100%"),
            tags$source(
              src = song_url_val,
              type = "audio/mp3"
            ),
            "Your browser does not support the audio element."
          )
        )
      )
    }

    # Function to add or update bird info card on the map
    update_bird_info_card <- function() {
      # Get current reactive values
      species_name_val <- selected_species()
      photo_url_val <- photo_url()
      scientific_name_val <- scientific_name()
      wiki_link_val <- wiki_link()
      song_url_val <- song_url()

      # Check if all required info is available
      req(species_name_val, photo_url_val, scientific_name_val, wiki_link_val, song_url_val)

      # Create the HTML content for the card
      card_html <- create_bird_info_card(
        species_name = species_name_val,
        scientific_name_val = scientific_name_val,
        wiki_url_val = wiki_link_val,
        photo_url_val = photo_url_val,
        song_url_val = song_url_val
      )

      if (!is.null(card_html)) {
        # Add/update the card to the map
        leafletProxy(ns("rasterMap")) |>
          # Remove existing card first to prevent duplicates if ID changes somehow
          removeControl(layerId = "bird-info-card") |>
          addControl(
            html = card_html,
            position = "topleft",
            layerId = "bird-info-card"
          )
        info_card_visible(TRUE) # Mark card as visible
      } else {
        # If data is missing, remove the card
        leafletProxy(ns("rasterMap")) |>
          removeControl(layerId = "bird-info-card")
        info_card_visible(FALSE) # Mark card as not visible
      }
    }

    # Update photo/wiki/song based on the selected species from the sidebar
    observeEvent(selected_species(),
      {
        # Ensure bird species info and selection are available
        req(bird_spp_info(), selected_species())

        # Get the selected common name
        selected_common_name <- selected_species()

        # Find the details for the selected species
        species_details <- bird_spp_info() |>
          filter(common_name == selected_common_name) |>
          slice(1) # Take the first row if multiple matches (shouldn't happen with common_name)

        # Update reactive values if details are found
        if (nrow(species_details) > 0) {
          photo_url(species_details$photo_url)
          wiki_link(species_details$wiki_link) # Corrected column name
          scientific_name(species_details$scientific_name)
          song_url(species_details$song_url)
        } else {
          # Reset reactive values if details not found
          photo_url(NULL)
          wiki_link(NULL)
          scientific_name(NULL)
          song_url(NULL)
        }

        # Call update_bird_info_card AFTER details are potentially updated
        update_bird_info_card()
      },
      ignoreNULL = TRUE # Don't run when selection is initially NULL
    )

    # Automatically update the map when current_date changes
    observeEvent(current_date(), {
      update_map_with_frame()
    })

    # Base leaflet map
    output$rasterMap <- renderLeaflet({
      # Create the base map
      m <- leaflet() |>
        addProviderTiles("CartoDB.Positron") |>
        # Set view to focus on southern Finland where most observations are
        setView(lng = 25.0, lat = 62.0, zoom = 6) |>
        addControl(
          html = create_hover_info_display(),
          position = "topright",
          layerId = "hover-info-control"
        )

      # Removed initial check for bird info card - observeEvent handles this

      return(m)
    })

    # Function to update map for the current date
    update_map_with_frame <- function() {
      # Make sure we have required data
      if (is.null(species_data()) || is.null(current_date())) {
        print("No species data or current date available")
        return(FALSE)
      }

      # Use tryCatch around the entire function to prevent any unexpected errors
      tryCatch(
        {
          # Safe print function to avoid formatting issues
          safe_print <- function(...) {
            args <- list(...)
            msg <- paste0(args, collapse = "")
            cat(msg, "\n")
          }

          safe_print("Updating map for current date")

          # Get the current date and data path (with validation)
          date <- current_date()
          if (is.null(date)) {
            safe_print("Date is NULL")
            return(FALSE)
          }

          # Ensure date is properly formatted
          if (!inherits(date, "Date")) {
            date <- as.Date(date)
          }

          scientific_name <- species_data()$scientific_name
          if (is.null(scientific_name) || is.na(scientific_name) || scientific_name == "") {
            safe_print("Invalid scientific name")
            return(FALSE)
          }

          # Format the date for file path
          date_str <- format(date, "%Y-%m-%d")

          # Use data_paths to get the parquet file for the current date
          data_paths <- species_data()$data_paths
          date_key <- as.character(date)
          parquet_path <- NULL
          if (!is.null(data_paths) && date_key %in% names(data_paths)) {
            parquet_path <- data_paths[[date_key]]
          }

          if (is.null(parquet_path) || !file.exists(parquet_path)) {
            safe_print("No parquet file found for date: ", date_key)
            # Clear map layers but keep controls (like info card)
            leafletProxy(ns("rasterMap")) |>
              clearImages() |>
              clearShapes() |>
              clearGroup("Heat Map")
            return(FALSE)
          }

          safe_print("Reading parquet file: ", parquet_path)

          # Update map using leafletProxy for better performance
          proxy <- leafletProxy(ns("rasterMap")) |>
            clearImages() |>
            clearShapes() |>
            clearGroup("Heat Map") |>
            # Always clear legend and other controls before adding new ones
            removeControl(layerId = "intensity-legend") |>
            removeControl(layerId = "layer-control") |>
            removeControl(layerId = "date-display-control") |>
            removeControl(layerId = "error-message-control")

          # Read the parquet file directly
          tryCatch(
            {
              safe_print("Reading parquet file")
              points_data <- read_parquet(parquet_path)

              # --- Enhanced Data Validation ---
              if (is.null(points_data)) {
                safe_print("ERROR: read_parquet returned NULL")
                return(FALSE) # Stop processing if data is NULL
              }

              safe_print("Loaded parquet file with ", nrow(points_data), " rows and ", ncol(points_data), " columns.")

              if (ncol(points_data) > 0) {
                safe_print("Columns in parquet: ", paste(colnames(points_data), collapse = ", "))
              } else if (nrow(points_data) > 0) {
                safe_print("WARNING: Data has rows but zero columns?")
              }

              # Check if points_data is empty or has no columns before proceeding
              if (nrow(points_data) == 0 || ncol(points_data) == 0) {
                safe_print("INFO: Parquet file is valid but contains no data rows or columns. No heatmap to plot.")
                # Update date display, clear map layers, but don't stop with error
                proxy <- leafletProxy(ns("rasterMap")) |>
                  clearImages() |>
                  clearShapes() |>
                  clearGroup("Heat Map") |>
                  removeControl(layerId = "intensity-legend") |>
                  removeControl(layerId = "layer-control") |>
                  removeControl(layerId = "date-display-control") |>
                  removeControl(layerId = "error-message-control")

                # Add date display
                date_to_display <- format_date_for_display(date)
                proxy |>
                  addControl(
                    html = create_date_display(date_to_display),
                    position = "bottomleft",
                    layerId = "date-display-control"
                  )

                # Re-add info card if needed
                if (info_card_visible()) {
                  update_bird_info_card()
                }

                return(TRUE) # Indicate success, even though no heatmap was added
              }

              # Base map setup is done in renderLeaflet, proxy modifies it.
              # No need for: m <- leaflet() |> addProviderTiles(...) |> setView(...)

              # Only add bird data if we have points
              if (nrow(points_data) > 0) {
                # Get column names
                coord_cols <- c("longitude", "latitude", "intensity")

                # Check if required columns exist
                if (all(coord_cols %in% colnames(points_data))) {
                  # --- Log Coordinate Range ---
                  lon_range <- range(points_data$longitude, na.rm = TRUE)
                  lat_range <- range(points_data$latitude, na.rm = TRUE)
                  safe_print(
                    "Coordinate Range: Longitude [", lon_range[1], ", ", lon_range[2], "], ",
                    "Latitude [", lat_range[1], ", ", lat_range[2], "]"
                  )
                  # ---------------------------

                  # --- Further Intensity Validation ---
                  valid_intensities <- points_data$intensity[!is.na(points_data$intensity)]
                  heatmap_added <- FALSE # Flag to track if heatmap is added

                  if (length(valid_intensities) == 0) {
                    safe_print("WARNING: All intensity values are NA. Cannot plot heatmap.")
                  } else {
                    intensity_min <- min(valid_intensities, na.rm = TRUE)
                    intensity_max <- max(valid_intensities, na.rm = TRUE)
                    safe_print("Intensity range (non-NA): ", intensity_min, " to ", intensity_max)

                    if (intensity_max <= 0) {
                      safe_print("WARNING: Max intensity is not positive. Heatmap may not be visible or meaningful.")
                    } else {
                      # Debug intensities (Original log)
                      safe_print(
                        "Original Intensity range log: ",
                        min(points_data$intensity, na.rm = TRUE), " to ",
                        max(points_data$intensity, na.rm = TRUE)
                      )

                      # Use YlGnBu colormap from RColorBrewer
                      ylgnbu_colors <- colorRampPalette(brewer.pal(9, "YlGnBu"))(20)

                      # Create color function for intensity values
                      # Domain uses validated min/max
                      if (intensity_min == intensity_max) {
                        domain <- c(intensity_min, intensity_min + 0.000001)
                      } else {
                        domain <- c(intensity_min, intensity_max)
                      }

                      base_pal <- colorNumeric(ylgnbu_colors, domain = domain, na.color = NA)
                      pal_na <- function(x) {
                        col <- base_pal(x)
                        col[is.na(col)] <- "#00000000" # Transparent for NA values
                        col
                      }

                      # Add heatmap
                      proxy |>
                        addHeatmap(
                          data = points_data,
                          lng = ~longitude,
                          lat = ~latitude,
                          intensity = ~ intensity * 4.0,
                          blur = 18,
                          max = intensity_max * 4.0 * 0.8,
                          radius = 15,
                          minOpacity = 0.7,
                          gradient = rev(ylgnbu_colors),
                          group = "Heat Map"
                        )
                      heatmap_added <- TRUE # Set flag

                      # Add legend
                      proxy |> addLegend(
                        layerId = "intensity-legend",
                        position = "bottomright",
                        pal = base_pal,
                        values = domain,
                        title = "Observation intensity",
                        opacity = 1.0
                      )
                    }
                  }
                  # --- End Intensity Validation ---

                  # Add layer controls using proxy (always add controls)
                  proxy |>
                    addLayersControl(
                      baseGroups = c("Base Map"),
                      overlayGroups = c("Heat Map"), # Keep Heat Map group even if empty
                      options = layersControlOptions(collapsed = FALSE)
                    )

                  # Add debug message to the map UI
                  debug_msg <- paste0(
                    "Points: ", nrow(points_data),
                    " | Intensity: ",
                    if (length(valid_intensities) > 0) paste0(min(valid_intensities, na.rm = TRUE), " to ", max(valid_intensities, na.rm = TRUE)) else "NA",
                    " | Lon: ", lon_range[1], " to ", lon_range[2],
                    " | Lat: ", lat_range[1], " to ", lat_range[2]
                  )
                  proxy |>
                    addControl(
                      html = div(
                        style = "background: rgba(255,255,255,0.8); padding: 4px; font-size: 12px; border-radius: 4px; border: 1px solid #ccc;",
                        debug_msg
                      ),
                      position = "topleft",
                      layerId = "debug-info-control"
                    )

                  # Log success
                  safe_print("Successfully processed data for the map.")
                } else { # This else corresponds to `if (all(coord_cols %in% colnames(points_data)))`
                  safe_print("WARNING: Expected columns (longitude, latitude, intensity) not found in parquet file")
                  safe_print("Available columns: ", paste(colnames(points_data), collapse = ", "))
                }
              } else {
                safe_print("No data points available for visualization")
              }

              # Update date display using proxy
              date_to_display <- format_date_for_display(date)
              proxy |>
                addControl(
                  html = create_date_display(date_to_display),
                  position = "bottomleft",
                  layerId = "date-display-control"
                )

              # Re-add info card using the dedicated function if it should be visible
              # This ensures it stays on top after other layers are updated
              if (info_card_visible()) {
                update_bird_info_card()
              }

              # Remove debug message if no data
              proxy |>
                removeControl(layerId = "debug-info-control")

              return(TRUE)
            },
            error = function(e) {
              safe_print("Error updating map: ", e$message)

              # Return a basic map if there's an error - NO, use proxy
              proxy |>
                addControl(
                  html = create_error_display(e$message),
                  position = "topright",
                  layerId = "error-message-control"
                )
            }
          )
        },
        error = function(e) {
          # Handle any unexpected errors
          cat("Error in update_map_with_frame: ", e$message, "\n")
          return(FALSE)
        }
      )
    }

    # Replace any HTML() usage for hover info display
    create_hover_info_display <- function() {
      div(
        id = "hover-info",
        class = "map-hover-display d-none"
      )
    }

    # Replace HTML() usage for date display
    create_date_display <- function(date_str) {
      div(
        class = "map-date-display",
        strong("Date: "),
        span(date_str)
      )
    }

    # Replace HTML() usage for error display
    create_error_display <- function(error_message) {
      div(
        class = "alert alert-danger",
        role = "alert",
        error_message
      )
    }

    # Replace HTML() usage for status messages
    create_status_message <- function(message, type = "info") {
      div(
        class = paste0("alert alert-", type),
        role = "alert",
        message
      )
    }

    # Return the update_map_with_frame function for external use
    list(
      update_map_with_frame = update_map_with_frame
      # No need to return add_bird_info_card anymore
    )
  })
}

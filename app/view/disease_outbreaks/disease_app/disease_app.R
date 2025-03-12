box::use(
  shiny,
  bslib,
  htmltools[css],
  waiter[Waiter],
  leaflet,
  leaflet.extras,
  leafem,
  zip[zip],
  sf,
  raster,
  jsonlite,
  utils[read.csv],
  ggplot2,
  reshape2,
  gridExtra,
)

box::use(
  app / logic / waiter[waiter_text],
)

#' @export
disease_app_ui <- function(id, i18n) {
  ns <- shiny$NS(id)
  shiny$tagList(
    shiny$sidebarLayout(
      shiny$sidebarPanel(
        shiny$fileInput(
          ns("file"),
          "Upload a GeoTIFF file",
          accept = c(".tiff", ".tif")
        ),
        shiny$actionButton(ns("select_area"), "Select Area"),
        shiny$verbatimTextOutput(ns("converted_coordinates")), # Display converted coordinates
        shiny$actionButton(ns("select_release"), "Select Release Coordinate"),
        shiny$verbatimTextOutput(ns("release_coordinates")), # Display release coordinates
        shiny$actionButton(ns("select_fences"), "Select Fences Coordinate"),
        shiny$verbatimTextOutput(ns("fence_coordinates")), # Display release coordinates
        shiny$actionButton(ns("run_command"), "Run model"),
        shiny$verbatimTextOutput(ns("command_output")), # Display WSL command output
        shiny$hr(), # Add a horizontal line for visual separation
        shiny$sliderInput(
          ns("tick_slider"),
          "Select Time Step:",
          min = 0,
          max = 100, # This will be updated dynamically
          value = 0
        ),
        shiny$actionButton(ns("load_grid"), "Show epidemic statistics"),
        shiny$uiOutput(ns("download_zip_ui")),

        # Base Map and Shapes Layer Control
        shiny$hr(), # Separator
        shiny$h4("Map Layer Control"),
        shiny$checkboxGroupInput(
          ns("base_layers"),
          "Select Base Layers:",
          choices = c("TIFF Map" = "tiff_map", "Drawn Shapes" = "drawn_shapes"),
          selected = c("tiff_map", "drawn_shapes")
        ),

        # Raster Layer Control
        shiny$hr(), # Separator
        shiny$h4("Raster Layer Control"),
        shiny$checkboxGroupInput(
          ns("raster_layers"),
          "Select Raster Layers to Display:",
          choices = c("Infected" = "infected", "Resistant" = "resistant", "Susceptible" = "susceptible"),
          selected = c("infected", "resistant", "susceptible")
        ),

        # Heatmap Control
        shiny$hr(), # Separator
        shiny$h4("Heatmap Control"),
        shiny$checkboxGroupInput(
          ns("heatmap_layers"),
          "Select Heatmaps to Display:",
          choices = c("Infected" = "infected", "Resistant" = "resistant", "Susceptible" = "susceptible"),
          selected = c("infected", "resistant", "susceptible")
        ),
        width = 3
      ),
      shiny$mainPanel(
        leaflet$leafletOutput(ns("map"), height = "600px"),
        shiny$verbatimTextOutput(ns("shape_info")), # Display drawn shape info
        shiny$plotOutput(ns("histogram_plot"), height = "400px"),
        shiny$plotOutput(ns("grid_plot"), height = "800px")
      )
    )
  )
}

#' @export
disease_app_server <- function(id, tab_disease_selected) {
  shiny$moduleServer(id, function(input, output, session) {
    # Define waiter ----
    msg <- waiter_text(message = shiny$tags$h3("Loading data...", style = "color: #414f2f;"))
    w <- Waiter$new(
      html = msg,
      color = "rgba(256,256,256,0.9)"
    )

    ns <- session$ns

    # shiny$reactive expression to read and process the TIFF file
    tiffRaster <- shiny$reactive({
      shiny$req(input$file) # Ensure file is uploaded

      # Load the GeoTIFF file
      raster$raster(input$file$datapath)
    })

    # Render the initial Leaflet map
    output$map <- leaflet$renderLeaflet({
      shiny$req(tiffRaster())

      # Create a base leaflet map
      map <- leaflet$leaflet() |>
        leaflet$addTiles() |>
        leafem$addMouseCoordinates() |>
        leaflet$addRasterImage(
          tiffRaster(),
          opacity = 0.8,
          project = TRUE,
          colors = leaflet$colorNumeric(
            c("#EEDED1", "#EA6D20"),
            domain = raster$values(tiffRaster()),
            na.color = "transparent"
          ),
          group = "TIFF Map"
        ) |>
        leaflet$addLayersControl(
          overlayGroups = c("TIFF Map", "drawnShapes"),
          options = leaflet$layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet.extras$addDrawToolbar(
          targetGroup = "drawnShapes",
          polygonOptions = leaflet.extras$drawPolygonOptions(
            showArea = TRUE,
            shapeOptions = list(
              fillOpacity = 0.2, # fill
              opacity = 0.2 # border
            )
          ),
          rectangleOptions = leaflet.extras$drawRectangleOptions(
            shapeOptions = list(
              fillOpacity = 0.2, # fill
              opacity = 0.2 # border
            )
          ),
          circleOptions = FALSE, # Disable circle markers
          editOptions = leaflet.extras$editToolbarOptions(remove = TRUE),
          polylineOptions = leaflet.extras$drawPolylineOptions()
        ) |>
        leaflet$addMiniMap(toggleDisplay = TRUE)

      map
    })

    # Observe changes in layer visibility
    shiny$observe({
      # Toggle TIFF Map visibility
      if ("tiff_map" %in% input$base_layers) {
        leaflet$leafletProxy("map") |>
          leaflet$showGroup("TIFF Map")
      } else {
        leaflet$leafletProxy("map") |>
          leaflet$hideGroup("TIFF Map")
      }

      # Toggle Drawn Shapes visibility
      if ("drawn_shapes" %in% input$base_layers) {
        leaflet$leafletProxy("map") |>
          leaflet$showGroup("drawnShapes")
      } else {
        leaflet$leafletProxy("map") |>
          leaflet$hideGroup("drawnShapes")
      }
    })

    # shiny$reactive value to store drawn rectangle
    drawn_area <- shiny$reactiveVal(NULL)
    release_point <- shiny$reactiveVal(NULL)
    drawn_fences <- shiny$reactiveVal(NULL)
    #
    drawn_area_save <- shiny$reactiveVal(NULL)
    drawn_area_save_4326 <- shiny$reactiveVal(NULL)
    release_point_save <- shiny$reactiveVal(NULL)
    drawn_fences_save <- shiny$reactiveVal(NULL)

    # shiny$reactive values to store different grid data types
    infected_grid_data <- shiny$reactiveVal(NULL)
    resistant_grid_data <- shiny$reactiveVal(NULL)
    susceptible_grid_data <- shiny$reactiveVal(NULL)

    # shiny$reactive value to track if grid data has been loaded
    grid_data_loaded <- shiny$reactiveVal(FALSE)

    # 3D matrices to store grid data across all ticks
    infected_data <- NULL
    resistant_data <- NULL
    susceptible_data <- NULL

    # Available ticks for grid data
    available_ticks <- NULL

    # List to store secondary infection data across all ticks
    sec_inf_data <- NULL

    # Available ticks for secondary infection data
    available_sec_inf_ticks <- NULL

    # Capture drawn shapes
    shiny$observeEvent(input$map_draw_new_feature, {
      feature <- input$map_draw_new_feature

      if (feature$geometry$type == "Polygon") {
        # Extract coordinates of the rectangle
        coords <- feature$geometry$coordinates[[1]] # Extract the first ring of the polygon

        # Convert the coordinates list to a matrix
        coords_matrix <- do.call(rbind, lapply(coords, function(pt) c(pt[[1]], pt[[2]])))

        # Calculate the bounding box
        bbox <- list(
          min_x = min(coords_matrix[, 1]),
          min_y = min(coords_matrix[, 2]),
          max_x = max(coords_matrix[, 1]),
          max_y = max(coords_matrix[, 2])
        )

        # Store the bounding box
        drawn_area(bbox)
        drawn_fences(coords_matrix)
      } else if (feature$geometry$type == "Point") {
        # Store release point coordinates
        release_point_coords <- feature$geometry$coordinates
        release_point(list(
          longitude = release_point_coords[[1]],
          latitude = release_point_coords[[2]]
        ))
      }
    })

    # Convert rectangle to EPSG:3035 when "Select Area" is clicked
    shiny$observeEvent(input$select_area, {
      shiny$req(drawn_area())
      bbox <- drawn_area()

      # Create an sf object for the bounding box
      rectangle_sf <- sf$st_as_sf(
        data.frame(
          x = c(bbox$min_x, bbox$max_x, bbox$max_x, bbox$min_x, bbox$min_x),
          y = c(bbox$min_y, bbox$min_y, bbox$max_y, bbox$max_y, bbox$min_y)
        ),
        coords = c("x", "y"),
        crs = 4326
      ) # WGS84

      drawn_area_save_4326(rectangle_sf)

      # Transform to EPSG:3035
      rectangle_sf_proj <- sf$st_transform(rectangle_sf, crs = 3035)

      # Get the transformed bounding box
      coords <- sf$st_bbox(rectangle_sf_proj)
      converted_bbox <- list(
        coordinates = c(
          coords["xmin"],
          coords["ymin"],
          coords["xmax"],
          coords["ymax"]
        )
      )

      # Store converted_bbox for access by the WSL command
      drawn_area(converted_bbox)
      drawn_area_save(converted_bbox)

      shiny$showNotification(
        "The modeling area is selected",
        type = "message"
      )
    })

    # Convert polygon to EPSG:3035 when "fence_coordinates" is clicked
    shiny$observeEvent(input$select_fences, {
      shiny$req(drawn_fences())
      fences_coords <- drawn_fences()

      fences_coords = data.frame(x = fences_coords[, 1], y = fences_coords[, 2])

      # Create an sf object for the bounding box
      fences_coords <- sf$st_as_sf(fences_coords, coords = c("x", "y"), crs = 4326) # WGS84

      # Transform to EPSG:3035
      fences_coords <- sf$st_transform(fences_coords, crs = 3035)

      # Convert to JSON
      fences_coords <- jsonlite$toJSON(
        list(
          type = "Polygon",
          coordinates = fences_coords
        ),
        auto_unbox = TRUE
      )

      # Store converted_bbox for access by the WSL command
      drawn_fences(fences_coords)
      drawn_fences_save(fences_coords)

      shiny$showNotification(
        "The fencing area is selected",
        type = "message"
      )
    })

    # Convert point to EPSG:3035 when "Select Release Point" is clicked
    shiny$observeEvent(input$select_release, {
      shiny$req(release_point()) # Ensure a point has been selected

      # Retrieve the release point data
      point <- release_point()

      # Create an sf object for the point
      point_sf <- sf$st_as_sf(
        data.frame(
          x = point$longitude,
          y = point$latitude
        ),
        coords = c("x", "y"),
        crs = 4326
      ) # WGS84

      # Transform to EPSG:3035
      point_sf_proj <- sf$st_transform(point_sf, crs = 3035)

      # Get the transformed point coordinates
      coords <- sf$st_coordinates(point_sf_proj)
      converted_point <- list(
        #x = round(coords[1, "X"]),
        #y = round(coords[1, "Y"])
        x = coords[1, "X"],
        y = coords[1, "Y"]
      )

      # Store the converted point for later use
      release_point(converted_point)
      release_point_save(converted_point)

      shiny$showNotification(
        "The release point is selected",
        type = "message"
      )
    })

    # Observe load_grid button press
    shiny$observeEvent(input$load_grid, {
      # Ensure output directory exists
      if (!dir.exists("outputs/epi_stat_outputs")) {
        shiny$showNotification("Output directory does not exist", type = "error")
        return()
      }

      # Find all CSV files for grid data
      infected_files <- list.files(
        "outputs/epi_stat_outputs",
        pattern = "^epi_stat_infected_tick_\\d+\\.csv$",
        full.names = TRUE
      )
      resistant_files <- list.files(
        "outputs/epi_stat_outputs",
        pattern = "^epi_stat_resistant_tick_\\d+\\.csv$",
        full.names = TRUE
      )
      susceptible_files <- list.files(
        "outputs/epi_stat_outputs",
        pattern = "^epi_stat_susceptible_tick_\\d+\\.csv$",
        full.names = TRUE
      )

      # Extract tick numbers
      infected_ticks <- as.numeric(gsub(".*_tick_(\\d+)\\.csv", "\\1", infected_files))
      resistant_ticks <- as.numeric(gsub(".*_tick_(\\d+)\\.csv", "\\1", resistant_files))
      susceptible_ticks <- as.numeric(gsub(".*_tick_(\\d+)\\.csv", "\\1", susceptible_files))

      # Find common ticks across all grid types
      available_ticks <<- sort(Reduce(intersect, list(infected_ticks, resistant_ticks, susceptible_ticks)))

      # If no common ticks, show error
      if (length(available_ticks) == 0) {
        shiny$showNotification("No common ticks found across grid types", type = "error")
        return()
      }

      # Read first file to determine grid dimensions
      first_infected_file <- infected_files[match(available_ticks[1], infected_ticks)]
      sample_data <- as.matrix(read.csv(first_infected_file, header = FALSE))

      # Initialize 3D matrices
      infected_data <<- array(NA_real_, dim = c(nrow(sample_data), ncol(sample_data), length(available_ticks)))
      resistant_data <<- array(NA_real_, dim = c(nrow(sample_data), ncol(sample_data), length(available_ticks)))
      susceptible_data <<- array(NA_real_, dim = c(nrow(sample_data), ncol(sample_data), length(available_ticks)))

      # Load data into 3D matrices
      for (i in seq_along(available_ticks)) {
        tick <- available_ticks[i]

        # Find corresponding files for this tick
        infected_file <- infected_files[match(tick, infected_ticks)]
        resistant_file <- resistant_files[match(tick, resistant_ticks)]
        susceptible_file <- susceptible_files[match(tick, susceptible_ticks)]

        # Read and store grid data
        infected_data[,, i] <<- as.matrix(read.csv(infected_file, header = FALSE))
        resistant_data[,, i] <<- as.matrix(read.csv(resistant_file, header = FALSE))
        susceptible_data[,, i] <<- as.matrix(read.csv(susceptible_file, header = FALSE))
        #print(paste("Loaded tick:", tick))
      }

      # Update tick slider
      shiny$updateSliderInput(
        session,
        "tick_slider",
        min = min(available_ticks),
        max = max(available_ticks),
        value = min(available_ticks)
      )

      # Set grid data as loaded
      grid_data_loaded(TRUE)

      # Show success notification
      shiny$showNotification("results loaded", type = "message")

      # Find all secondary infection CSV files
      sec_inf_files <- list.files(
        "outputs/sec_inf_outputs",
        pattern = "^secondary_infections_tick_\\d+\\.csv$",
        full.names = TRUE
      )

      # Extract tick numbers for secondary infection files
      sec_inf_ticks <- as.numeric(gsub(".*_tick_(\\d+)\\.csv", "\\1", sec_inf_files))

      # Sort ticks
      available_sec_inf_ticks <<- sort(sec_inf_ticks)

      # If no secondary infection ticks found, show error
      if (length(available_sec_inf_ticks) == 0) {
        shiny$showNotification("No secondary infection data found", type = "error")
        return()
      }

      # Initialize list to store secondary infection data
      sec_inf_data <<- vector("list", length(available_sec_inf_ticks))
      names(sec_inf_data) <<- as.character(available_sec_inf_ticks)

      # Load secondary infection data
      for (i in seq_along(available_sec_inf_ticks)) {
        tick <- available_sec_inf_ticks[i]
        file <- sec_inf_files[match(tick, sec_inf_ticks)]

        # Read CSV file
        sec_inf_data[[i]] <<- read.csv(file, header = TRUE)
      }
    })

    # Update raster layers when tick slider changes
    shiny$observe({
      # shiny$require grid data to be loaded and 3D matrices to exist
      shiny$req(grid_data_loaded(), !is.null(infected_data))

      # Find the index of the current tick
      current_tick_index <- which(available_ticks == input$tick_slider)

      if (length(current_tick_index) > 0) {
        # Extract grid data for the current tick
        current_infected_grid <- infected_data[,, current_tick_index]
        current_resistant_grid <- resistant_data[,, current_tick_index]
        current_susceptible_grid <- susceptible_data[,, current_tick_index]

        # Update grid data shiny$reactive values
        infected_grid_data(current_infected_grid)
        resistant_grid_data(current_resistant_grid)
        susceptible_grid_data(current_susceptible_grid)
      }
    })

    # Update histogram when tick slider changes
    shiny$observeEvent(
      input$tick_slider,
      {
        #print(paste("Tick slider changed to:", input$tick_slider))

        # shiny$require secondary infection data to be loaded
        shiny$req(!is.null(sec_inf_data), length(sec_inf_data) > 0)

        # Find the index of the current tick
        current_sec_inf_index <- which(as.numeric(names(sec_inf_data)) == input$tick_slider)

        if (length(current_sec_inf_index) > 0) {
          # Extract secondary infection data for the current tick
          current_sec_inf <- sec_inf_data[[current_sec_inf_index]]

          # Prepare data for plotting
          infection_types <- c("total", "within", "between", "carcass")

          # Color mapping for infection types
          infection_colors <- c(
            "total" = "#ff6666",
            "within" = "#ff9999",
            "between" = "#66b3ff",
            "carcass" = "#99ff99"
          )

          # Render histogram plot
          output$histogram_plot <- shiny$renderPlot({
            # Only render if grid data has been loaded
            shiny$req(grid_data_loaded())

            # Prepare plots
            plots <- list()

            # Create separate histograms for each infection type
            for (infection_type in infection_types) {
              # Prepare data for the specific infection type
              hist_data <- data.frame(
                Secondary_Infections = 0:(nrow(current_sec_inf) - 1),
                Frequency = current_sec_inf[[infection_type]]
              )

              # Create histogram if there's data
              if (nrow(hist_data) > 0) {
                histogram_plot <- ggplot2$ggplot(hist_data, ggplot2$aes(x = Secondary_Infections, y = Frequency)) +
                  ggplot2$geom_bar(stat = "identity", fill = infection_colors[infection_type]) +
                  ggplot2$labs(
                    title = paste("Secondary", infection_type, "Infections Histogram"),
                    x = "Number of Secondary Infections",
                    y = "Frequency"
                  ) +
                  ggplot2$theme_minimal() +
                  ggplot2$theme(
                    plot.title = ggplot2$element_text(hjust = 0.5),
                    legend.position = "none"
                  )

                # Add plot to the list
                plots[[paste0("Secondary_Infections_", infection_type)]] <- histogram_plot
              }
            }

            # Arrange plots
            if (length(plots) > 0) {
              if (length(plots) == 1) {
                plots[[1]]
              } else {
                gridExtra$grid.arrange(grobs = plots, ncol = 2)
              }
            }
          })
        } else {
          print(paste("No secondary infection data found for tick", input$tick_slider))
        }
      },
      ignoreNULL = FALSE
    )

    # Update raster layers when tick slider changes
    shiny$observe({
      # Only update if grid data has been initially loaded
      shiny$req(grid_data_loaded())

      # Get bounding box from drawn area
      bbox <- sf$st_bbox(drawn_area_save_4326())

      # Clear existing layers
      leaflet$leafletProxy("map") |>
        leaflet$clearGroup("Infected Grid") |>
        leaflet$clearGroup("Resistant Grid") |>
        leaflet$clearGroup("Susceptible Grid") |>
        leaflet$clearGroup("Release Point")

      infected_raster <- NULL
      resistant_raster <- NULL
      susceptible_raster <- NULL
      # Add infected grid data layer if available and contains non-zero values
      if (
        !is.null(infected_grid_data()) &&
          any(infected_grid_data() > 0) &&
          "infected" %in% input$raster_layers
      ) {
        # Create raster with correct extent for infected grid
        infected_raster <- raster$raster(
          infected_grid_data(),
          xmn = bbox$xmin,
          xmx = bbox$xmax,
          ymn = bbox$ymin,
          ymx = bbox$ymax,
          crs = sf$st_crs(4326)$proj4string
        )

        # Add the infected grid data raster to the map
        leaflet$leafletProxy("map") |>
          leaflet$addRasterImage(
            infected_raster,
            opacity = 0.5,
            colors = leaflet$colorBin(
              c("white", "red"),
              domain = c(-0.01, max(raster$values(infected_raster), na.rm = TRUE)),
              bins = 4,
              na.color = "transparent"
            ),
            group = "Infected Grid"
          )
      }

      # Add resistant grid data layer if available and contains non-zero values
      if (
        !is.null(resistant_grid_data()) &&
          any(resistant_grid_data() > 0) &&
          "resistant" %in% input$raster_layers
      ) {
        # Create raster with correct extent for resistant grid
        resistant_raster <- raster$raster(
          resistant_grid_data(),
          xmn = bbox$xmin,
          xmx = bbox$xmax,
          ymn = bbox$ymin,
          ymx = bbox$ymax,
          crs = sf$st_crs(4326)$proj4string
        )

        # Add the resistant grid data raster to the map
        leaflet$leafletProxy("map") |>
          leaflet$addRasterImage(
            resistant_raster,
            opacity = 0.5,
            colors = leaflet$colorBin(
              c("white", "green"),
              domain = c(-0.01, max(raster$values(resistant_raster), na.rm = TRUE)),
              bins = 4,
              na.color = "transparent"
            ),
            group = "Resistant Grid"
          )
      }

      # Add susceptible grid data layer if available and contains non-zero values
      if (
        !is.null(susceptible_grid_data()) &&
          any(susceptible_grid_data() > 0) &&
          "susceptible" %in% input$raster_layers
      ) {
        # Create raster with correct extent for susceptible grid
        susceptible_raster <- raster$raster(
          susceptible_grid_data(),
          xmn = bbox$xmin,
          xmx = bbox$xmax,
          ymn = bbox$ymin,
          ymx = bbox$ymax,
          crs = sf$st_crs(4326)$proj4string
        )

        # Add the susceptible grid data raster to the map
        leaflet$leafletProxy("map") |>
          leaflet$addRasterImage(
            susceptible_raster,
            opacity = 0.5,
            colors = leaflet$colorBin(
              palette = c("white", "blue"), # Changed color to blue for susceptible
              domain = c(-0.01, max(raster$values(susceptible_raster), na.rm = TRUE)),
              bins = 4,
              na.color = "transparent"
            ),
            group = "Susceptible Grid"
          )
      }

      # Add release point if available
      if (!is.null(release_point())) {
        # Safely extract longitude and latitude
        release_lon <- tryCatch(
          release_point()$longitude,
          error = function(e) NULL
        )
        release_lat <- tryCatch(
          release_point()$latitude,
          error = function(e) NULL
        )

        # Only add marker if both coordinates are valid
        if (!is.null(release_lon) && !is.null(release_lat)) {
          leaflet$leafletProxy("map") |>
            leaflet$addMarkers(
              lng = release_lon,
              lat = release_lat,
              popup = "Release Point",
              group = "Release Point"
            )
        }
      }

      # Add legends for existing grid layers
      # Clear any existing legends first
      leaflet$leafletProxy("map") |>
        leaflet$removeControl("infected_legend") |>
        leaflet$removeControl("resistant_legend") |>
        leaflet$removeControl("susceptible_legend")

      # Track legend positions
      legend_position <- "bottomright"
      legend_count <- 0

      # Check and add legend for infected raster
      if (!is.null(infected_raster)) {
        leaflet$leafletProxy("map") |>
          leaflet$addLegend(
            layerId = "infected_legend",
            position = legend_position,
            pal = leaflet$colorBin(
              palette = c("white", "red"),
              domain = c(-0.01, max(raster$values(infected_raster), na.rm = TRUE)),
              bins = 4
            ),
            values = raster$values(infected_raster),
            title = "Infected Grid",
            group = "Infected Grid",
            opacity = 0.7
          )
        legend_count <- legend_count + 1
      }

      # Check and add legend for resistant raster
      if (!is.null(resistant_raster)) {
        leaflet$leafletProxy("map") |>
          leaflet$addLegend(
            layerId = "resistant_legend",
            position = legend_position,
            pal = leaflet$colorBin(
              palette = c("white", "green"),
              domain = c(-0.01, max(raster$values(resistant_raster), na.rm = TRUE)),
              bins = 4
            ),
            values = raster$values(resistant_raster),
            title = "Resistant Grid",
            group = "Resistant Grid",
            opacity = 0.7
          )
        legend_count <- legend_count + 1
      }

      # Check and add legend for susceptible raster
      if (!is.null(susceptible_raster)) {
        leaflet$leafletProxy("map") |>
          leaflet$addLegend(
            layerId = "susceptible_legend",
            position = legend_position,
            pal = leaflet$colorBin(
              palette = c("white", "blue"),
              domain = c(-0.01, max(raster$values(susceptible_raster), na.rm = TRUE)),
              bins = 4
            ),
            values = raster$values(susceptible_raster),
            title = "Susceptible Grid",
            group = "Susceptible Grid",
            opacity = 0.7
          )
        legend_count <- legend_count + 1
      }

      # Optional: Adjust legend position if multiple legends exist
      if (legend_count > 1) {
        legend_positions <- c(
          "bottomright",
          "bottomleft",
          "topright",
          "topleft"
        )
        legend_position <- legend_positions[1:legend_count]
      }
    })

    # Render the grid plot
    output$grid_plot <- shiny$renderPlot({
      # Only render if grid data has been loaded
      shiny$req(grid_data_loaded())

      # Prepare plots
      plots <- list()

      # Define color palettes
      color_palette <- c(
        "Susceptible" = "#66b3ff",
        "Infected" = "#ff6666",
        "Resistant" = "#99ff99"
      )

      # Read grid data based on selected layers
      layer_names <- c(
        "Susceptible" = "susceptible",
        "Infected" = "infected",
        "Resistant" = "resistant"
      )

      # Use heatmap_layers for selection instead of raster_layers
      selected_layers <- names(layer_names)[layer_names %in% input$heatmap_layers]

      # Create heatmaps for selected layers
      for (layer_name in selected_layers) {
        grid_data <- switch(
          layer_name,
          "Susceptible" = susceptible_grid_data(),
          "Infected" = infected_grid_data(),
          "Resistant" = resistant_grid_data()
        )

        if (!is.null(grid_data)) {
          # Create heatmap
          heatmap_plot <- ggplot2$ggplot(data = reshape2$melt(grid_data)) +
            ggplot2$geom_tile(ggplot2$aes(x = Var2, y = Var1, fill = value)) +
            ggplot2$scale_fill_gradient(low = "white", high = color_palette[layer_name]) +
            ggplot2$scale_y_reverse() +
            ggplot2$labs(title = paste(layer_name, "Grid"), x = "X", y = "Y", fill = "Count") +
            ggplot2$theme_minimal() +
            ggplot2$theme(legend.position = "right")

          plots[[layer_name]] <- heatmap_plot
        }
      }

      # Arrange plots
      if (length(plots) > 0) {
        if (length(plots) == 1) {
          plots[[1]]
        } else {
          gridExtra$grid.arrange(grobs = plots, ncol = 2)
        }
      }
    })

    # Add UI element for zip export
    output$download_zip_ui <- shiny$renderUI({
      shiny$actionButton("export_zip", "Export Outputs", icon = shiny$icon("file-zipper"))
    })

    # Observe export zip button press
    shiny$observeEvent(input$export_zip, {
      # Ensure outputs directory exists
      shiny$req(dir.exists("outputs"))

      # Generate filename with timestamp
      file_name <- paste0("outputs_", format(Sys.time(), "%Y%m%d%H%M%S"), ".zip")

      zip(zipfile = file_name, files = "outputs")

      if (file.exists(file_name)) {
        shiny$showNotification(
          paste("The outputs are successfully exported to", file_name),
          type = "message"
        )
      }
    })

    # Run WSL Command
    shiny$observeEvent(input$run_command, {
      # Clear grid data
      infected_grid_data(NULL)
      resistant_grid_data(NULL)
      susceptible_grid_data(NULL)

      # Reset grid data loaded flag
      grid_data_loaded(FALSE)

      # Show a notification
      shiny$showNotification("Modelling started.", type = "message")

      shiny$req(input$file) # Ensure a file is uploaded

      # Extract the file name
      map_name <- basename(input$file$name)

      # Retrieve parameters
      converted_bbox <- drawn_area_save()
      area <- paste("[", paste(converted_bbox$coordinates, collapse = ", "), "]", sep = "")

      release_coord <- paste("[", paste(as.numeric(release_point_save()), collapse = ", "), "]", sep = "")

      fence_polygon <- drawn_fences_save()

      wsl_command <- sprintf(
        'docker run -e INPUT_MAP=%s -e COMPUTED_AREA=%s -e RELEASE_COORDS=%s -e FENCE_COORDS=%s -e OUTPUT_DIR="/code/outputs" -v "$(pwd)/outputs:/code/outputs" asf_dckr python /code/experiments/shiny.py',
        shQuote(map_name),
        shQuote(area),
        shQuote(release_coord),
        shQuote(fence_polygon)
      )

      print(wsl_command)
      # Run the command and capture output
      tryCatch(
        {
          command_output <- system(wsl_command)
          output$command_output <- shiny$renderPrint({
            command_output
          })
        },
        error = function(e) {
          output$command_output <- shiny$renderPrint({
            paste("Error executing WSL command:", e$message)
          })
        }
      )
    })
  })
}

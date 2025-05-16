box::use(
  shiny,
  bslib,
  htmltools[css],
  waiter[Waiter],
  leaflet,
  leaflet.extras,
  zip[zip],
  jsonlite,
  terra,
  readr,
  echarty[ecs.render, ecs.output],
  shinyjs[hide, show, hidden, delay, disabled, disable, enable],
)

box::use(
  app / logic / waiter[waiter_text],
  app / logic / disease_outbreaks / disease_data_load[load_simulated_data],
  app / logic / disease_outbreaks / disease_histogram[disease_histogram],
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
        disabled(shiny$actionButton(ns("run_command"), "Run model")),
        shiny$verbatimTextOutput(ns("command_output")), # Display WSL command output
        shiny$hr(), # Add a horizontal line for visual separation
        hidden(
          shiny$sliderInput(
            ns("tick_slider"),
            "Select Time Step:",
            min = -1,
            max = 0, # This will be updated dynamically
            value = -1,
            step = 1
          )
        ),
        hidden(
          shiny$actionButton(
            ns("export_zip"),
            "Export Outputs",
            icon = shiny$icon("file-zipper")
          )
        ),
        width = 3,
        shiny$uiOutput(ns("statusMsg")),
      ),
      shiny$mainPanel(
        leaflet$leafletOutput(ns("map"), height = "600px"),
        ecs.output(ns("histogram_plot"), height = "400px"),
        shiny$verbatimTextOutput(ns("shape_info")) # Display drawn shape info
      )
    )
  )
}

#' @export

disease_app_server <- function(
  id,
  tab_disease_selected,
  session_dir
) {
  shiny$moduleServer(id, function(input, output, session) {
    # Define waiter ----
    msg <- waiter_text(message = shiny$tags$h3("Loading data...", style = "color: #414f2f;"))
    w <- Waiter$new(
      html = msg,
      color = "rgba(256,256,256,0.9)"
    )

    # Prepare directory for results ----
    # Non-persistent data solution
    # Making a beekeeper dir in the shared folder
    temp_dir <- session_dir |>
      file.path("disease_outbreaks")

    r_disease <- shiny$reactiveValues(
      bounds = NULL,
      release_point = NULL,
      fences = NULL,
      tiff_raster = NULL,
      r_dir = NULL,
      infected_files = NULL,
      infected_data = NULL,
      infected_ticks = NULL,
      susceptible_files = NULL,
      susceptible_data = NULL,
      susceptible_ticks = NULL,
      resistant_files = NULL,
      resistant_data = NULL,
      resistant_ticks = NULL,
      available_ticks = NULL,
      sec_inf_ticks = NULL,
      sec_inf_files = NULL,
      sec_inf_data = NULL,
      run_success = 0,
      run_dir = NULL
    )

    ns <- session$ns

    shiny$observeEvent(
      tab_disease_selected(),
      {
        output$map <- leaflet$renderLeaflet(
          leaflet$leaflet() |>
            leaflet$addTiles() |>
            leaflet$setView(
              lng = 9,
              lat = 53,
              zoom = 4
            )
        )
      }
    )

    output$statusMsg <- shiny$renderUI({
      shiny$div(
        class = "alert alert-info",
        role = "alert",
        "Upload GeoTIFF file and select desired area by dragging a rectangle. Mark the release point by using marker and fence the area by using polygon."
      )
    })

    # shiny$reactive expression to read and process the TIFF file
    shiny$observeEvent(
      input$file,
      {
        shiny$req(input$file) # Ensure file is uploaded
        r_disease$tiff_raster <- terra$rast(input$file$datapath)
      }
    )

    # Render the initial Leaflet map
    shiny$observeEvent(
      r_disease$tiff_raster,
      {
        shiny$req(r_disease$tiff_raster)

        # Create a base leaflet map
        map <- leaflet$leafletProxy("map") |>
          leaflet$addTiles() |>
          leaflet$addRasterImage(
            r_disease$tiff_raster,
            opacity = 0.8,
            project = TRUE,
            colors = leaflet$colorNumeric(
              c("#EEDED1", "#EA6D20"),
              domain = terra$values(r_disease$tiff_raster),
              na.color = "transparent"
            ),
            group = "Input Map"
          ) |>
          leaflet$addLayersControl(
            overlayGroups = c(
              "Input Map",
              "Bounds",
              "Fences",
              "Release Point"
            ),
            options = leaflet$layersControlOptions(collapsed = FALSE)
          ) |>
          leaflet.extras$addDrawToolbar(
            targetGroup = "drawnShapes",
            polygonOptions = leaflet.extras$drawPolygonOptions(
              showArea = TRUE,
              shapeOptions = list(
                fillOpacity = 0.2, # fill
                opacity = 0.2, # border
                color = "#a20101",
                fillColor = "#a20101"
              )
            ),
            rectangleOptions = leaflet.extras$drawRectangleOptions(
              shapeOptions = list(
                fillOpacity = 0.2, # fill
                opacity = 0.2 # border
              )
            ),
            circleMarkerOptions = FALSE, # Disable circle markers
            circleOptions = FALSE, # Disable circle markers
            editOptions = leaflet.extras$editToolbarOptions(remove = TRUE),
            polylineOptions = FALSE
          ) |>
          # This is to hide the drawn shapes layer by default
          # Apparently, it is not easy to delete the drawnShapes without whole draw toolbar
          leaflet$hideGroup("drawnShapes")

        map
      }
    )

    shiny$observeEvent(
      input$map_draw_new_feature,
      {
        shiny$req(input$map_draw_new_feature)
        feature <- input$map_draw_new_feature
        feature_type <- input$map_draw_new_feature$properties$feature_type
        coords <- input$map_draw_new_feature$geometry$coordinates

        leaflet$leafletProxy("map") |>
          leaflet$removeShape(layerId = feature$properties$layerId)

        if (feature_type == "rectangle") {
          leaflet$leafletProxy("map") |>
            leaflet$removeShape("Bounds") |>
            leaflet$addGeoJSON(
              feature,
              group = "Bounds",
              layerId = "Bounds",
              output$statusMsg <- shiny$renderUI({
                shiny$div(
                  class = "alert alert-info",
                  role = "alert",
                  "Area selected"
                )
              })
            )

          helper_bbox <- terra::ext(
            coords[[1]][[1]][[1]], # xmin
            coords[[1]][[3]][[1]], # xmax
            coords[[1]][[1]][[2]], # ymin
            coords[[1]][[3]][[2]] # ymax
          ) |>
            terra$project("EPSG:4326", "EPSG:3035")
          r_disease$bounds <- c(
            helper_bbox[1],
            helper_bbox[3],
            helper_bbox[2],
            helper_bbox[4]
          )
        } else if (feature_type == "polygon") {
          leaflet$leafletProxy("map") |>
            leaflet$removeShape("Fences") |>
            leaflet$addGeoJSON(
              feature,
              group = "Fences",
              layerId = "Fences",
              fillOpacity = 0.2, # fill
              opacity = 0.2, # border
              color = "#a20101",
              fillColor = "#a20101",
              output$statusMsg <- shiny$renderUI({
                shiny$div(
                  class = "alert alert-info",
                  role = "alert",
                  "Fences selected"
                )
              })
            )

          # Convert the coordinates list to a matrix
          coords_matrix <- do.call(rbind, lapply(coords[[1]], function(pt) c(pt[[1]], pt[[2]])))

          # Create SpatVector from coordinates matrix
          poly_vect <- terra::vect(coords_matrix, type = "polygon", crs = "EPSG:4326")

          # Project to EPSG:3035
          poly_proj <- terra::project(poly_vect, "EPSG:3035") |>
            terra$geom()

          poly_proj_list <-
            apply(poly_proj[, c("x", "y")], 1, function(x) {
              list(geometry = list(type = "Point", coordinates = x))
            })

          r_disease$fences <- jsonlite::toJSON(
            list(
              type = "Polygon",
              coordinates = poly_proj_list
            ),
            auto_unbox = TRUE
          )
        } else if (feature_type == "marker") {
          leaflet$leafletProxy("map") |>
            leaflet$removeShape("Release Point") |>
            leaflet$addGeoJSON(
              feature,
              group = "Release Point",
              layerId = "Release Point",
              output$statusMsg <- shiny$renderUI({
                shiny$div(
                  class = "alert alert-info",
                  role = "alert",
                  "Releasing point selected"
                )
              })
            )
          temp <- terra$vect(
            matrix(
              c(
                coords[[1]],
                coords[[2]]
              ),
              ncol = 2
            ),
            type = "point",
            crs = "EPSG:4326"
          ) |>
            terra$project(
              "EPSG:3035"
            ) |>
            terra$geom()
          r_disease$release_point <- temp[, c("x", "y")]
        }
      }
    )

    shiny$observe({
      # Check if bounds, release_point, and fences are set
      all_set <- !is.null(r_disease$bounds) &&
        !is.null(r_disease$release_point) &&
        !is.null(r_disease$fences)

      if (all_set) {
        enable("run_command")
      } else {
        disable("run_command")
      }
    })

    # Run WSL Command
    shiny$observeEvent(
      input$run_command,
      ignoreInit = TRUE,
      {
        shiny$req(
          r_disease$bounds,
          r_disease$release_point,
          r_disease$fences
        )

        r_disease$run_dir <- file.path(
          temp_dir,
          Sys.time() |> format(format = "%Y-%m-%d-%H-%M-%S")
        )
        dir.create(r_disease$run_dir, recursive = TRUE)

        # Show a notification
        shiny$showNotification("Modelling started.", type = "message")

        output$statusMsg <- shiny$renderUI({
          shiny$div(
            class = "alert alert-info",
            role = "alert",
            "Please wait, modelling started"
          )
        })

        shiny$req(input$file) # Ensure a file is uploaded

        # Extract the file name
        map_name <- basename(input$file$name)

        # Retrieve parameters
        area <- paste("[", paste(r_disease$bounds, collapse = ", "), "]", sep = "")
        release_coord <- paste("[", paste(r_disease$release_point, collapse = ", "), "]", sep = "")
        fence_polygon <- r_disease$fences

        file.copy(input$file$datapath, file.path(r_disease$run_dir, "map.tif"))

        wsl_command <- sprintf(
          'docker run -e INPUT_MAP="map.tif" -e COMPUTED_AREA=%s -e RELEASE_COORDS=%s -e FENCE_COORDS=%s -e OUTPUT_DIR="/code/outputs" -v "%s:/code/outputs" asf_dckr python /code/experiments/shiny.py',
          shQuote(area),
          shQuote(release_coord),
          shQuote(fence_polygon),
          r_disease$run_dir
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
        if (dir.exists(file.path(r_disease$run_dir, "epi_stat_outputs"))) {
          r_disease$run_success <- r_disease$run_success + 1
        }
      }
    )

    shiny$observeEvent(
      r_disease$run_success,
      ignoreInit = TRUE,
      {
        shiny$req(r_disease$run_dir)

        load_simulated_data(
          r_disease$run_dir,
          r_disease
        )

        output$statusMsg <- shiny$renderUI({
          shiny$div(
            class = "alert alert-info",
            role = "alert",
            "Modelling successfull"
          )
        })

        show("tick_slider")
        show("export_zip")

        delay(
          200,
          {
            # Update tick slider
            shiny$updateSliderInput(
              session,
              "tick_slider",
              min = min(r_disease$available_ticks),
              max = max(r_disease$available_ticks),
              value = min(r_disease$available_ticks)
            )
          }
        )
      }
    )

    # Update histogram and map output when tick slider changes
    shiny$observeEvent(
      {
        input$tick_slider
      },
      ignoreInit = TRUE,
      {
        hist_data <- r_disease$sec_inf_data[[as.character(input$tick_slider)]]
        # Prepare data for secondary infection histogram
        output$histogram_plot <- ecs.render(
          disease_histogram(hist_data)
        )
        helper_susceptible <-
          r_disease$susceptible_data[[
            as.character(input$tick_slider)
          ]]
        helper_infected <-
          r_disease$infected_data[[
            as.character(input$tick_slider)
          ]]
        helper_resistant <-
          r_disease$resistant_data[[
            as.character(input$tick_slider)
          ]]
        # Remove old rasters and add new ones ----
        leaflet$leafletProxy("map") |>
          leaflet$clearGroup("Infected Grid") |>
          leaflet$clearGroup("Resistant Grid") |>
          leaflet$clearGroup("Susceptible Grid") |>
          leaflet$addRasterImage(
            helper_susceptible,
            group = "Susceptible Grid",
            colors = leaflet$colorNumeric(
              "Blues",
              domain = terra$values(helper_susceptible),
              na.color = "transparent"
            ),
            opacity = 0.4,
            project = FALSE
          ) |>
          leaflet$addRasterImage(
            helper_infected,
            group = "Infected Grid",
            colors = leaflet$colorNumeric(
              "Reds",
              domain = terra$values(helper_infected),
              na.color = "transparent"
            ),
            opacity = 0.4,
            project = FALSE
          ) |>
          leaflet$addRasterImage(
            helper_resistant,
            group = "Resistant Grid",
            colors = leaflet$colorNumeric(
              "Greens",
              domain = terra$values(helper_resistant),
              na.color = "transparent"
            ),
            opacity = 0.4,
            project = FALSE
          ) |>
          leaflet$addLayersControl(
            overlayGroups = c(
              "Input Map",
              "Bounds",
              "Fences",
              "Release Point",
              "Susceptible Grid",
              "Infected Grid",
              "Resistant Grid"
            ),
            options = leaflet$layersControlOptions(collapsed = FALSE)
          ) |>
          leaflet$addLegend(
            pal = leaflet$colorNumeric(
              "Blues",
              domain = terra$values(helper_susceptible),
              na.color = "transparent"
            ),
            values = terra$values(helper_susceptible),
            opacity = 0.4,
            group = "Susceptible Grid",
            layerId = "susceptible_legend",
            position = "bottomright"
          ) |>
          leaflet$addLegend(
            pal = leaflet$colorNumeric(
              "Reds",
              domain = terra$values(helper_infected),
              na.color = "transparent"
            ),
            values = terra$values(helper_infected),
            opacity = 0.4,
            group = "Infected Grid",
            layerId = "infected_legend",
            position = "bottomright"
          ) |>
          leaflet$addLegend(
            pal = leaflet$colorNumeric(
              "Greens",
              domain = terra$values(helper_resistant),
              na.color = "transparent"
            ),
            values = terra$values(helper_resistant),
            opacity = 0.4,
            group = "Resistant Grid",
            layerId = "resistant_legend",
            position = "bottomright"
          )
      }
    )

    # Observe export zip button press
    shiny$observeEvent(input$export_zip, {
      # Ensure outputs directory exists
      shiny$req(dir.exists(r_disease$run_dir))

      # Generate filename with timestamp
      file_name <- paste0("outputs_", format(Sys.time(), "%Y%m%d%H%M%S"), ".zip")

      zip(zipfile = file_name, files = r_disease$run_dir)

      if (file.exists(file_name)) {
        shiny$showNotification(
          paste("The outputs are successfully exported to", file_name),
          type = "message"
        )
      }
    })
  })
}

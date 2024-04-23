box::use(
  shiny[moduleServer, NS, tags, actionButton, observeEvent, req, reactive, reactiveVal],
  bslib[card, card_header],
  shinyjs[disabled, disable, enable],
  purrr[is_empty],
  terra[vect, project, buffer, crop, writeRaster],
  fs[file_copy],
  utils[write.csv],
)

box::use(
  app/logic/waiter[waiter_text],
)

#' @export
beekeeper_control_ui <- function(id) {
  ns <- NS(id)
  card(
    id = ns("control"),
    class = "mt-2 mx-md-3 card-shadow",
    card_header(
      tags$div(
        class = "row d-flex justify-content-between align-items-center my-1",
        tags$div(
          class = "col-md-8 col-sm-12 me-auto",
          tags$h5("Honeybee Beekeeper Case"),
        ),
        tags$div(
          class = "col-md-4 col-sm-12 d-flex flex-row justify-content-end",
          disabled(
            actionButton(
              ns("run_simulation"),
              label = "Run simulation",
              width = "100%",
              class = "btn-secondary",
              style = "max-width: 200px"
            )
          )
        )
      )
    )
  )
}

#' @export
beekeeper_control_server <- function(
    id,
    coordinates,
    lookup,
    parameters,
    landuse_map,
    session_dir) {
  moduleServer(id, function(input, output, session) {
    # Define waiter ----
    waiter_text(message = tags$h3("Computing Beehave simulation...",
                                  style = "color: #414f2f;"
    ))
    
    w <- Waiter$new(
      html = msg[[1]],
      color = "rgba(256,256,256,0.9)"
    )
    
    # Prepare directory for results ----
    # Non-persistent data solution
    # Making a beekeeper dir in the shared folder
    temp_dir <- session_dir |>
      file.path("beekeeper")

    experiment_list <- reactiveVal(
      c(Example = "app/data/honeybee/output_example/Result_table_original.csv")
    )
    counter <- 0

    # Run workflow button ----
    observeEvent(
      coordinates(),
      {
        if (!is_empty(coordinates()) &
          !is_empty(lookup()) &
          !is_empty(parameters())) {
          enable("run_simulation")
        } else {
          disable("run_simulation")
        }
      }
    )

    # Workflow execution ----
    observeEvent(
      input$run_simulation,
      {
        # Start waiter ----
        w$update(
          html = waiter_text(message = tags$h3("Running simulation...",
            style = "color: #414f2f;"
          ))
        )
        w$show()

        # Check data ----
        req(
          coordinates(),
          lookup(),
          parameters()
        )

        counter <- counter + 1

        # Prepare folder structure ----
        if (!dir.exists(session_dir)) {
          dir.create(session_dir)
        }
        if (!dir.exists(temp_dir)) {
          dir.create(temp_dir)
        }

        run_dir <- file.path(
          temp_dir,
          Sys.time() |> format(format = "%Y-%m-%d_%H-%M-%S")
        )
        dir.create(run_dir)

        lookup_file <- file.path(run_dir, "lookup_table.csv")
        parameters_file <- file.path(run_dir, "parameters.csv")
        locations_file <- file.path(run_dir, "locations.csv")
        simulation_file <- file.path(run_dir, "simulation.csv")
        map_file <- file.path(run_dir, "map.tif")

        # Prepare input data ----
        bee_location <- coordinates() |>
          vect(
            geom = c("lon", "lat"),
            crs = "EPSG:4326"
          ) |>
          project(landuse_map())

        # create buffer around Beehave Location
        clip_buffer <- buffer(bee_location,
          width = 5000
        )
        # ... and clip raster to buffer
        location_area <- crop(
          landuse_map(),
          clip_buffer
        )

        # BEWARE !!!!!!!!!!!!
        # HARDCODED paths follows
        
        file_copy(file.path("app", "data", "honeybee", "Beehave_BeeMapp2015_Netlogo6version_PolygonAggregation.nlogo"),
          file.path(run_dir, "Beehave_BeeMapp2015_Netlogo6version_PolygonAggregation.nlogo"),
          overwrite = TRUE
        )

        writeRaster(
          location_area,
          map_file
        )

        write.csv(lookup(),
          file = lookup_file,
          row.names = FALSE
        )

        write.csv(parameters()$parameters,
          file = parameters_file,
          row.names = FALSE
        )

        write.csv(parameters()$simulation,
          file = simulation_file,
          row.names = FALSE
        )

        write.csv(
          data.frame(
            id = 1
          ) |>
            cbind(coordinates()),
          file = locations_file,
          row.names = FALSE
        )
        # Run workflow ----
        # docker_call <- paste0("shared/uc-pollinators/scripts/cloud/cloud_execution.sh shared/uc-pollinators/R ", run_dir, " shared/uc-pollinators/scripts/cloud")
        # Execute docker run, no socket should be needed for this
        docker_call <- paste0('docker run -v "', Sys.getenv("SCRIPT_PATH"), '":"/scripts" -v "', Sys.getenv("R_PATH"), '":"/R" -v "', paste0(Sys.getenv("DATA_PATH"), stringr::str_remove(run_dir, paste0(Sys.getenv("HOME_PATH"), "shared"))), '":"/data" -e INPUT_DIR="/data" -e OUTPUT_DIR="/data/output" -e MAP="map.tif" -e LOOKUP_TABLE="lookup_table.csv" -e LOCATIONS="locations.csv" -e PARAMETERS="parameters.csv" -e MODEL_PATH="/data/Beehave_BeeMapp2015_Netlogo6version_PolygonAggregation.nlogo" -e CPUS="1" --cpus 1 --platform linux/amd64 --entrypoint /scripts/run_docker_flow.sh ghcr.io/biodt/beehave:0.3.8')
        
        system(docker_call)

        # Update output data ----
        new_out <- file.path(run_dir, "output", "output_id1_iter1.csv")
        names(new_out) <- paste("Run", counter)
        if (file.exists(new_out)) {
          new_list <- experiment_list() |>
            c(new_out)
          experiment_list(new_list)
        }

        # Hide waiter ----
        w$hide()
      }
    )

    reactive(experiment_list())
  })
}

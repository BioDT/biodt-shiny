box::use(
  shiny[moduleServer, NS, tags, tagList, actionButton, observeEvent, req, reactive, reactiveVal],
  bslib[card, card_header, card_body],
  shinyjs[disabled, disable, enable],
  purrr[is_empty],
  terra[vect, project, buffer, crop, writeRaster],
  fs[file_copy],
  utils[write.csv],
  waiter[Waiter],
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
    ),
    card_body(
      id = ns("control"),
      tags$div(
        class = "row d-flex justify-content-between",
        tags$h5("Instructions"),
        tags$ol(
          tags$style("
            ol li {
              margin-bottom: 10px;
              margin-left: 20px;
            }
          "),
          tags$li("Select point on the map"),
          tags$li("Adjust the parameters"),
          tags$li("Change the lookup table values if needed"),
          tags$li("Click the run simulation button")
        ),
        tags$p("The simulation results can be seen in the output plot, select your experiment from the dropdown menu.")
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
    msg <- waiter_text(
      message = tagList(
        tags$h3("Computing Beehave simulation...",
          style = "color: #414f2f;"
        ),
        tags$br(),
        tags$h4("This operation takes some time.",
          style = "color: #414f2f;"
        ),
        tags$h4("You can expect it to run for 2 to 4 minutes.",
          style = "color: #414f2f;"
        ),
        tags$h4("Please do not close the tab during this time. You can browse other tabs.",
          style = "color: #414f2f;"
        )
      ),
    )

    w <- Waiter$new(
      html = msg,
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
    counter <- reactiveVal(0)

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
        w$show()

        # Check data ----
        req(
          coordinates(),
          lookup(),
          parameters()
        )

        counter(counter() + 1)

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
        # docker_call <- paste0('docker run -v "/Users/martinovic/resilio/IT4I/Projects/BioDT/WP6/uc-pollinators/scripts/cloud/":"/scripts" -v "/Users/martinovic/resilio/IT4I/Projects/BioDT/WP6/uc-pollinators/R":"/R" -v "/Users/martinovic/git/biodt-shiny/', run_dir,'":"/data" -e INPUT_DIR="/data" -e OUTPUT_DIR="/data/output" -e MAP="map.tif" -e LOOKUP_TABLE="lookup_table.csv" -e LOCATIONS="locations.csv" -e PARAMETERS="parameters.csv" -e NETLOGO_JAR_PATH="/NetLogo 6.3.0/lib/app/netlogo-6.3.0.jar" -e MODEL_PATH="/data/Beehave_BeeMapp2015_Netlogo6version_PolygonAggregation.nlogo" -e CPUS="1" --cpus 1 --platform linux/amd64 --entrypoint /scripts/run_docker_flow.sh ghcr.io/biodt/beehave:0.3.9')
        # Execute docker run, no socket should be needed for this

        docker_call <- paste0('docker run -v "', Sys.getenv("SCRIPT_PATH"), '":"/scripts" -v "', Sys.getenv("R_PATH"), '":"/R" -v "', paste0(Sys.getenv("DATA_PATH"), stringr::str_remove(run_dir, paste0(Sys.getenv("HOME_PATH"), "shared"))), '":"/data" -e INPUT_DIR="/data" -e OUTPUT_DIR="/data/output" -e MAP="map.tif" -e LOOKUP_TABLE="lookup_table.csv" -e LOCATIONS="locations.csv" -e PARAMETERS="parameters.csv" -e NETLOGO_JAR_PATH="/NetLogo 6.3.0/lib/app/netlogo-6.3.0.jar" -e MODEL_PATH="/data/Beehave_BeeMapp2015_Netlogo6version_PolygonAggregation.nlogo" -e CPUS="1" --cpus 1 --platform linux/amd64 --entrypoint /scripts/run_docker_flow.sh ghcr.io/biodt/beehave:0.3.9')

        system(docker_call)

        # Update output data ----
        new_out <- file.path(run_dir, "output", "output_id1_iter1.csv")
        names(new_out) <- paste("Run", counter())
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

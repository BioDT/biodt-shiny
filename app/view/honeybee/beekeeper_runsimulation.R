box::use(
  shiny[NS, moduleServer, tags, tagList, fluidRow, actionButton, reactiveVal, observeEvent, reactive, req],
  bslib[card, card_header, card_body],
  shinyjs[disabled, disable, enable],
  terra[vect, project, buffer, crop, writeRaster],
  purrr[is_empty],
  waiter[Waiter],
  fs[file_copy],
  utils[write.csv],
  httr2[request, req_headers, req_body_json, req_url_path_append, req_perform, req_method, req_options, req_perform_connection, resp_check_status, resp_stream_lines],
  jsonlite[fromJSON],
)

box::use(
  app / logic / waiter[waiter_text],
)

create_and_wait_k8s_job <- function(data_subpath, run_id) {
  # Get the service account token
  token <- Sys.getenv("KUBERNETES_API_TOKEN", "")
  if ( token == "" ) {
    print("No manual token present, reading k8s-mounted service account token")
    token <- readLines("/var/run/secrets/kubernetes.io/serviceaccount/token", warn = FALSE)
  }
  api_url <- Sys.getenv("KUBERNETES_API_URL", "https://kubernetes.default.svc")
  namespace <- Sys.getenv("KUBERNETES_NAMESPACE", "default")
  shinyproxy_id <- Sys.getenv("SHINYPROXY_ID", "")
  if (shinyproxy_id == "") {
    stop("SHINYPROXY_ID is not set, and is mandatory for k8s-based execution.")
  }
  
  # Generate job name
  job_name <- paste0("beehave-", tolower(gsub("[^[:alnum:]]", "-", shinyproxy_id)), "-", run_id)

  # Job specification
  job_spec <- list(
    apiVersion = "batch/v1",
    kind = "Job",
    metadata = list(
      name = job_name,
      namespace = namespace
    ),
    spec = list(
      template = list(
        spec = list(
          containers = list(
            list(
              name = "beehave",
              image = "ghcr.io/biodt/beehave:0.3.9",
              command = list("/scripts/cloud/run_docker_flow.sh"),
              env = list(
                list(name = "INPUT_DIR", value = "/data"),
                list(name = "OUTPUT_DIR", value = "/data/output"),
                list(name = "MAP", value = "map.tif"),
                list(name = "LOOKUP_TABLE", value = "lookup_table.csv"),
                list(name = "LOCATIONS", value = "locations.csv"),
                list(name = "PARAMETERS", value = "parameters.csv"),
                list(name = "NETLOGO_JAR_PATH", value = "/NetLogo 6.3.0/lib/app/netlogo-6.3.0.jar"),
                list(name = "MODEL_PATH", value = "/data/Beehave_BeeMapp2015_Netlogo6version_PolygonAggregation.nlogo"),
                list(name = "CPUS", value = "1")
              ),
              resources = list(
                requests = list(cpu = "1"),
                limits = list(cpu = "1")
              ),
              volumeMounts = list(
                list(name = "biodt-scripts-volume", mountPath = "/scripts/cloud"),
                list(name = "biodt-pollinators-r-volume", mountPath = "/R"),
                list(name = paste0("biodt-shared-dir-volume-", shinyproxy_id), mountPath = "/data", subPath = data_subpath)
              )
            )
          ),
          volumes = list(
            list(
              name = "biodt-scripts-volume",
              persistentVolumeClaim = list(claimName = "biodt-scripts-pvc")
            ),
            list(
              name = "biodt-pollinators-r-volume",
              persistentVolumeClaim = list(claimName = "biodt-pollinators-r-pvc")
            ),
            list(
              name = paste0("biodt-shared-dir-volume-", shinyproxy_id),
              persistentVolumeClaim = list(claimName = paste0("biodt-shared-dir-pvc-", shinyproxy_id))
            )
          ),
          restartPolicy = "Never"
        )
      ),
      backoffLimit = 4
    )
  )
  
  # Create the job
  print("Creating a k8s-based beehave job...")
  response <- request(api_url) |>
    req_url_path_append("apis", "batch", "v1", "namespaces", namespace, "jobs") |>
    req_headers(
      "Authorization" = paste("Bearer", token),
      "Content-Type" = "application/json"
    ) |>
    req_options(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE) |>
    req_body_json(job_spec) |>
    req_perform()
  
  resp_check_status(response)
  
  # Wait for job completion
  print("Waiting for beehave job completion...")
  timeout <- 1200  # 20 minutes timeout
  start_time <- Sys.time()

  watch_url <- paste0(api_url, "/apis/batch/v1/namespaces/", namespace, 
                     "/jobs?watch=true&timeoutSeconds=5000&fieldSelector=metadata.name=", job_name)

  # Set up streaming GET request
  tryCatch({
    con <- request(watch_url) |>
      req_headers("Authorization" = paste("Bearer", token)) |>
      req_method("GET") |>
      req_options(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE) |>
      req_perform_connection(blocking = FALSE, verbosity = NULL)
    
      while(TRUE) {
        # For some reason, the blocking connection (or the stream) in httr2 is not working
        # properly, (only the first line is parsed) so we need to use a non-blocking connection
        # and an active waiting loop to check if there is any data available.
        # TODO: Submit an issue to the httr2 package, or switch to old httr package.
        line <- resp_stream_lines(con, lines = 1, warn = FALSE)
        if (length(line) == 0) {
          Sys.sleep(0.1)
          next
        }

        event <- fromJSON(line)
        status <- event$object$status
        
        if (!is.null(status$succeeded) && status$succeeded > 0) {
          print("Beehave job completed successfully! Stopping the watch...")
          break
        }
        if (!is.null(status$failed) && status$failed > 0) {
          stop("FAILED")
        }
        if (difftime(Sys.time(), start_time, units = "secs") > timeout) {
          stop("TIMEOUT")
        }
        next
      }
  }, error = function(e) {
    if(grepl("FAILED", e$message)) {
      stop("Beehave job failed.")
    }
    else if(grepl("TIMEOUT", e$message)) {
      stop("Beehave job timeout.")
    }
    else{
      error_message <- paste0("Unexpected error: ", e)
      stop(error_message)
    }
  })
  close(con)
  print("Successfully stopped the watch.")

  # Delete the job
  job_delete_response <- request(api_url) |>
    req_url_path_append("apis", "batch", "v1", "namespaces", namespace, "jobs", job_name) |>
    req_method("DELETE") |>
    req_headers(
      "Authorization" = paste("Bearer", token),
      "Content-Type" = "application/json"
    ) |>
    req_options(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE) |>
    req_body_json(list(propagationPolicy = "Foreground")) |>
    req_method("DELETE") |>
    req_perform()

  tryCatch({
    resp_check_status(job_delete_response)
    print("Beehave job deleted successfully.")
  }, error = function(e) {
    print("Failed to delete Beehave job: ", e$message)
  })

  return(TRUE)
}

#' @export
beekeeper_runsimulation_ui <- function(id, i18n) {
  ns <- NS(id)
  card(
    id = ns("runsimulation"),
    class = "mt-2 mx-md-3 card-shadow overflow-hidden",
    card_header(
      tags$div(
        class = "row d-flex justify-content-between align-items-center my-1",
        tags$div(
          class = "col-md-8 col-sm-12 me-auto",
          tags$h2(
            class = "card_title",
            i18n$translate("Start simulation")
          ),
        ),
        tags$div(
          class = "col-md-4 col-sm-12 d-flex flex-row justify-content-end",
          disabled(
            actionButton(
              ns("run_simulation"),
              label = i18n$translate("Run simulation"),
              width = "100%",
              class = "btn-secondary btn-lg",
              style = "max-width: 300px"
            )
          )
        )
      )
    ),
  )
}

#' @export
beekeeper_runsimulation_server <- function(
    id,
    coordinates,
    lookup,
    parameters,
    landuse_map,
    session_dir) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
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

    # this variable represents all runs of simulation in order in which they were
    # chronologicaly run by user; "Example" is dummy hardcoded example for
    # demonstration purpose
    experiment_list <- reactiveVal(
      c(Example = "app/data/honeybee/output_example/Result_table_original.csv")
    )
    counter <- reactiveVal(0)

    # Run workflow button ----
    observeEvent(
      coordinates(),
      ignoreNULL = FALSE,
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
        data_subpath <- stringr::str_remove(run_dir, paste0(Sys.getenv("HOME_PATH"), "shared/"))

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
        print("Starting the workflow execution.")
        EXECUTOR_TYPE <- Sys.getenv("EXECUTOR_TYPE", "DOCKER")
        run_id <- counter()

        if (EXECUTOR_TYPE == "DOCKER") {
          docker_call <- paste0('docker run -v "', Sys.getenv("SCRIPT_PATH"), '":"/scripts" -v "', Sys.getenv("R_PATH"), '":"/R" -v "', paste0(Sys.getenv("DATA_PATH"), stringr::str_remove(run_dir, paste0(Sys.getenv("HOME_PATH"), "shared"))), '":"/data" -e INPUT_DIR="/data" -e OUTPUT_DIR="/data/output" -e MAP="map.tif" -e LOOKUP_TABLE="lookup_table.csv" -e LOCATIONS="locations.csv" -e PARAMETERS="parameters.csv" -e NETLOGO_JAR_PATH="/NetLogo 6.3.0/lib/app/netlogo-6.3.0.jar" -e MODEL_PATH="/data/Beehave_BeeMapp2015_Netlogo6version_PolygonAggregation.nlogo" -e CPUS="1" --cpus 1 --platform linux/amd64 --entrypoint /scripts/run_docker_flow.sh ghcr.io/biodt/beehave:0.3.9')
          system(docker_call)
        } else if (EXECUTOR_TYPE == "KUBERNETES") {
          create_and_wait_k8s_job(data_subpath, run_id)
        } else {
          stop("Invalid executor type: ", EXECUTOR_TYPE)
        }
        print("Workflow execution completed.")
        # Update output data ----
        new_out <- file.path(run_dir, "output", "output_id1_iter1.csv")
        names(new_out) <- paste("Run", run_id)
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

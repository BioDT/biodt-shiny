box::use(
  httr2[
    request,
    req_headers,
    req_body_json,
    req_url_path_append,
    req_perform,
    req_method,
    req_options,
    req_perform_connection,
    resp_check_status,
    resp_stream_lines
  ],
  jsonlite[fromJSON],
)

create_and_wait_k8s_job <- function(data_subpath, run_id) {
  # Get the service account token
  token <- Sys.getenv("KUBERNETES_API_TOKEN", "")
  if (token == "") {
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
              image = "ghcr.io/biodt/beehave:0.3.13",
              command = list("/scripts/cloud/run_docker_flow.sh"),
              env = list(
                list(name = "INPUT_DIR", value = "/data"),
                list(name = "OUTPUT_DIR", value = "/data/output"),
                list(name = "MAP", value = "map.tif"),
                list(name = "LOOKUP_TABLE", value = "lookup_table.csv"),
                list(name = "LOCATIONS", value = "locations.csv"),
                list(name = "PARAMETERS", value = "parameters.csv"),
                list(name = "NETLOGO_JAR_PATH", value = "/NetLogo 6.3.0/lib/app/netlogo-6.3.0.jar"),
                list(
                  name = "MODEL_PATH",
                  value = "/data/Beehave_BeeMapp2015_Netlogo6version_PolygonAggregation.nlogo"
                ),
                list(name = "CPUS", value = "1")
              ),
              resources = list(
                limits = list(cpu = "1")
              ),
              volumeMounts = list(
                list(
                  name = paste0("biodt-shared-dir-volume-", shinyproxy_id),
                  mountPath = "/data",
                  subPath = data_subpath
                )
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
              persistentVolumeClaim = list(
                claimName = paste0("biodt-shared-dir-pvc-", shinyproxy_id)
              )
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
  timeout <- 1200 # 20 minutes timeout
  start_time <- Sys.time()

  watch_url <- paste0(
    api_url,
    "/apis/batch/v1/namespaces/",
    namespace,
    "/jobs?watch=true&timeoutSeconds=5000&fieldSelector=metadata.name=",
    job_name
  )

  # Set up streaming GET request
  tryCatch(
    {
      con <- request(watch_url) |>
        req_headers("Authorization" = paste("Bearer", token)) |>
        req_method("GET") |>
        req_options(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE) |>
        req_perform_connection(blocking = FALSE, verbosity = NULL)

      while (TRUE) {
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
    },
    error = function(e) {
      if (grepl("FAILED", e$message)) {
        stop("Beehave job failed.")
      } else if (grepl("TIMEOUT", e$message)) {
        stop("Beehave job timeout.")
      } else {
        error_message <- paste0("Unexpected error: ", e)
        stop(error_message)
      }
    }
  )
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

  tryCatch(
    {
      resp_check_status(job_delete_response)
      print("Beehave job deleted successfully.")
    },
    error = function(e) {
      print("Failed to delete Beehave job: ", e$message)
    }
  )

  return(TRUE)
}

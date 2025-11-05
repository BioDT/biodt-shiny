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
    req_timeout,
    resp_check_status,
    resp_stream_lines
  ],
  jsonlite[fromJSON, toJSON],
)

#' @export
create_and_wait_k8s_job <- function(
  data_subpath,
  run_id,
  lat,
  lon,
  start_year,
  end_year,
  deimsid,
  cdsapi_url,
  cdsapi_key,
  hda_user,
  hda_password
) {
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
  job_name <- paste0("grassland-", tolower(gsub("[^[:alnum:]]", "-", shinyproxy_id)), "-", run_id)

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
              name = "grassland",
              image = "ghcr.io/biodt/uc-grassland:latest",
              command = list("/bin/bash", "-c", "cd /uc-grassland-model && ./run_pipeline_uc_grassland.sh"),
              env = list(
                list(name = "LAT", value = as.character(lat)),
                list(name = "LON", value = as.character(lon)),
                list(name = "startYear", value = as.character(start_year)),
                list(name = "endYear", value = as.character(end_year)),
                list(name = "DEIMS", value = as.character(deimsid)),
                list(name = "CDSAPI_URL", value = as.character(cdsapi_url)),
                list(name = "CDSAPI_KEY", value = as.character(cdsapi_key)),
                list(name = "HDA_USER", value = as.character(hda_user)),
                list(name = "HDA_PASSWORD", value = as.character(hda_password))
              ),
              resources = list(
                limits = list(cpu = "2", memory = "4Gi"),
                requests = list(cpu = "1", memory = "2Gi")
              ),
              volumeMounts = list(
                list(
                  name = paste0("biodt-shared-dir-volume-", shinyproxy_id),
                  mountPath = "/output",
                  subPath = data_subpath
                )
              )
            )
          ),
          volumes = list(
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
  print("Creating a k8s-based grassland simulation job...")
  print(jsonlite::toJSON(job_spec, auto_unbox = TRUE, pretty = TRUE))
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
  print("Waiting for grassland simulation job completion...")
  timeout <- 1800 # 30 minutes timeout (grassland simulations can be longer)
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
        req_timeout(3600) |>
        req_perform_connection(blocking = FALSE, verbosity = NULL)

      while (TRUE) {
        # For some reason, the blocking connection (or the stream) in httr2 is not working
        # properly, (only the first line is parsed) so we need to use a non-blocking connection
        # and an active waiting loop to check if there is any data available.
        # TODO: Submit an issue to the httr2 package, or switch to old httr package.
        line <- character(0)
        tryCatch(
          {
            line <- resp_stream_lines(con, lines = 1, warn = FALSE)
          },
          error = function(e) {
            print(paste("Error reading stream: ", e$message))
            line <- character(0)
          }
        )
        if (length(line) == 0) {
          Sys.sleep(0.1)
          next
        }

        event <- fromJSON(line)
        status <- event$object$status

        if (!is.null(status$succeeded) && status$succeeded > 0) {
          print("Grassland simulation job completed successfully! Stopping the watch...")
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
        stop("Grassland simulation job failed.")
      } else if (grepl("TIMEOUT", e$message)) {
        stop("Grassland simulation job timeout.")
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
      print("Grassland simulation job deleted successfully.")
    },
    error = function(e) {
      print(paste("Failed to delete grassland simulation job: ", e$message))
    }
  )

  return(TRUE)
}

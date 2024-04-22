box::use(
  uuid[UUIDvalidate],
  httr2[request, req_perform, resp_body_json],
  purrr[map_dfr],
  dplyr[filter, mutate],
  stringr[str_extract_all],
)

is_uuid <- function(id) {
  UUIDvalidate(id)
}

resp_site <- function(deimsid) {
  site <- request("https://deims.org/api/sites") |>
    req_perform() |>
    resp_body_json() |>
    map_dfr(~.x) |>
    filter(id != "https://deims.org/") |>
    mutate(id = as.character(id)) |>
    filter(id == deimsid)

  return(site)
}

parse_site <- function(site) {
  coords <- site$coordinates |> str_extract_all("[0-9]+\\.[0-9]+")

  coords_parsed <- list()
  coords_parsed$lng <- as.numeric(coords[[1]][1])
  coords_parsed$lat <- as.numeric(coords[[1]][2])

  return(coords_parsed)
}

#' @export
get_coords_deimsid <- function(deimsid) {
  if (is_uuid(deimsid)) {
    site <- resp_site(deimsid)
    coordinates <- parse_site(site)
  } else {
    coordinates <- NA
  }

  return(coordinates)
}

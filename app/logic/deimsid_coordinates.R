library(dplyr)
library(httr2)
library(stringr)

resp_site <- function(deimsid) {
  site <- httr2::request("https://deims.org/api/sites") |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::map_dfr(~.x) |>
    dplyr::filter(id != "https://deims.org/") |>
    dplyr::mutate(id = as.character(id)) |>
    dplyr::filter(id == deimsid)

  return(site)
}

parse_site <- function(site) {
  coords <- site$coordinates |> stringr::str_extract_all("[0-9]+\\.[0-9]+")

  coords_parsed <- list()
  coords_parsed$lat <- as.numeric(coords[[1]][1])
  coords_parsed$lng <- as.numeric(coords[[1]][2])
  
  return(coords_parsed)
}

#' @export
get_coords <- function(deimsid) {
  site <- resp_site(deimsid)
  coordinates <- parse_site(site)

  return(coordinates)
}
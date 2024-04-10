library(dplyr)
library(httr2)

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

# example output:::
# > resp_site("102ae489-04e3-481d-97df-45905837dc1a")
# # A tibble: 1 × 4
#   title                              id                      changed coordinates
#   <chr>                              <chr>                   <chr>   <chr>      
# 1 TERENO - Bad Lauchstaedt - Germany 102ae489-04e3-481d-97d… 2023-0… POINT (11.…


# TODO - finish funs below, in Shiny call only 
parse_site <- function(site) {

  location <- list() 
  location$lng <- 0
  location$lat <- 0

 # return(location)
}

get_coords <- function(deimsid) {
  site <- resp_site(deimsid)
  coords_parsed <- parse_site(site)

  return(coords_parsed)
}
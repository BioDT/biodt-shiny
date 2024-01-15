## code to prepare `CWR` dataset goes here

CWR <- terra::rast("data-raw/Lathyrus_sativus-Outputs.nc") |>
  leaflet::projectRasterForLeaflet(method = "bilinear")
usethis::use_data(CWR, overwrite = TRUE)

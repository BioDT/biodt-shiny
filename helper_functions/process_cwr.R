
# Define variables
source_path <- "~/data/cwr"
target_path <- "~/git/biodt-shiny/app/data/cwr"
rasters <- list.files(source_path, "MODELS-Binarised.nc", recursive = TRUE)
# Define post processing function
post_process_cwr <- function(
  input_path,
  source_path,
  target_path
) {
  # Loading of the NetCDF file, averaging it and projecting it
  temp <- terra::rast(
    file.path(source_path, input_path)) |>
    terra::mean(na.rm = TRUE) |>
    terra::project("EPSG:3857")
  # Binarisation of the raster
  output <- terra::ifel(temp > 0.6, 1, 0)
  # Prepare file names and directories
  temp_dir <- gsub(
    "/MODELS-Binarised.nc",
    "",
    input_path
  )
  output_dir <- 
    file.path(
      target_path,
      temp_dir
    )
  print(output_dir)

  if(!dir.exists(output_dir)) {
    dir.create(
      output_dir,
      recursive = TRUE
    )
  }
  # Write results as tif
  terra::writeRaster(
    output,
    file.path(
      output_dir,
      "MODELS-Binarised.tif"
    ),
    overwrite = TRUE
  )
}
# Execute post-processing
purrr::walk(
  rasters[1],
  post_process_cwr,
  source_path,
  target_path
)

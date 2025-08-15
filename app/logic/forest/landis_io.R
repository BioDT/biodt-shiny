# Load the libraries
box::use(
  stringr,
  utils
)

#' Read LANDIS parameter files from a simulation directory
#'
#' Expects at least `PnET-succession.txt` to be present in `dir`.
#' If `scenario.txt` exists, tries to parse Duration from it.
#'
#' Robust to extra spaces and Windows line endings.
#' Returns list(start_year = int|NA, timestep = int|NA, duration = int|NA).
#' @export
read_landis_params <- function(dir) {
  succ_path <- file.path(dir, "PnET-succession.txt")
  scen_path <- file.path(dir, "scenario.txt")

  # helper to normalize lines
  norm_lines <- function(lines) {
    if (length(lines) == 0) return(character(0))
    lines <- gsub("\r", "", lines, fixed = TRUE)
    trimws(lines)
  }

  # helper to pull first integer value for a key
  extract_int <- function(lines, key_regex) {
    if (length(lines) == 0) return(NA_integer_)
    hit <- grep(key_regex, lines, value = TRUE)
    if (length(hit) == 0) return(NA_integer_)
    val <- stringr$str_extract(hit[1], "\\d+")
    as.integer(val)
  }
  succ_lines <- character(0)
  if (file.exists(succ_path)) {
    succ_lines <- tryCatch(readLines(succ_path, warn = FALSE), error = function(e) character(0))
    succ_lines <- norm_lines(succ_lines)
  }

  scen_lines <- character(0)
  if (file.exists(scen_path)) {
    scen_lines <- tryCatch(readLines(scen_path, warn = FALSE), error = function(e) character(0))
    scen_lines <- norm_lines(scen_lines)
  }

  start_year <- extract_int(succ_lines, "^StartYear\\b")
  timestep   <- extract_int(succ_lines, "^Timestep\\b")
  duration   <- extract_int(scen_lines, "^Duration\\b")

  list(start_year = start_year, timestep = timestep, duration = duration)
}

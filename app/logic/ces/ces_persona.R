# File:       persona.R
# Package:    biodt.recreation
# Repository: https://github.com/BioDT/uc-ces-recreation
# License:    MIT
# Copyright:  2025 BioDT and the UK Centre for Ecology & Hydrology
# Author(s):  Joe Marsh Rossney

box::use(
  biodt.recreation[
    load_config,
    assert_to_bool,
  ],
)

box::use(
  app /
    logic /
    ces /
    ces_persona_utils[
      capture_messages,
      errors_as_messages
    ],
)

assert_valid_persona <- function(persona) {
  expected_names <- load_config()[["Name"]]
  if (!identical(sort(names(persona)), sort(expected_names))) {
    stop("Error: malformed names in the persona vector")
  }
  if (!all(persona == floor(persona))) {
    stop("Error: persona contains non-integer values")
  }
  if (all(sapply(persona, function(score) score == 0))) {
    stop(paste(
      "All the persona scores are zero. At least one score must be non-zero.",
      "Perhaps you have forgotten to load a persona?"
    ))
  }
  return(TRUE)
}

is_valid_persona <- function(persona) {
  assert_to_bool(assert_valid_persona)(persona)
}

#' @export
check_valid_persona <- function(persona) {
  capture_messages(errors_as_messages(assert_valid_persona))(persona)
}


list_csv_files <- function(dir) {
  return(list.files(path = dir, pattern = "\\.csv$", full.names = FALSE))
}

list_personas_in_file <- function(persona_file) {
  # TODO: replace with call to read_persona_csv?
  personas <- names(utils::read.csv(persona_file, nrows = 1))
  return(personas[personas != "index"])
}

get_persona_dir <- function() {
  system.file("extdata", "personas", package = "biodt.recreation", mustWork = TRUE)
}

#' Get Preset Persona File
#'
#' Get the path to the CSV file containing preset personas.
#'
#' @returns Path to the preset persona file.
#'
#' @export
get_preset_persona_file <- function() {
  system.file("extdata", "personas", "presets.csv", package = "biodt.recreation", mustWork = TRUE)
}

#' Read Persona CSV
#'
#' Read a CSV file containing one or more personas.
#'
#' This function essentially calls `readr::read_csv` with `col_types` set to reflect the
#' expected columns in a persona file. That is: one `character` column for the index,
#' and _at least_ one integer column for the persona scores.
#'
#' @param csv_path Path to a CSV file containing the persona(s)
#'
#' @returns data.frame containing _at least_ two columns: the `index` column and the persona(s)
#'
#' @seealso [biodt.recreation::load_persona()] loads a single persona.
#'
#' @example inst/examples/read_persona_csv.R
#'
#' @keywords internal
#' @export
read_persona_csv <- function(csv_path) {
  # Read csv as a dataframe of integers, throwing an error for non-integer elements
  df <- readr::read_csv(
    csv_path,
    col_types = readr::cols(
      index = readr::col_character(),
      .default = readr::col_integer()
    )
  )
  # NOTE: this may be redundant given index specified in col_types
  if (!"index" %in% colnames(df)) {
    stop("Error: the file does not contain an index column")
  }

  if (!ncol(df) > 1) {
    stop("Error: the file does not contain any personas")
  }

  return(df)
}

#' Load Persona
#'
#' Load a single persona from a CSV file.
#'
#' Loads a single persona from a csv file containing one or more personas.
#' If the file contains more than one persona (i.e. columns), a name specifying
#' which personal (column) to load must also be provided.
#'
#' @param csv_path Path to a csv file containing one or more personas
#' @param name Name of the persona, which should match a column name
#'
#' @returns A named vector of integers representing the persona
#'
#' @seealso [biodt.recreation::read_persona_csv()] reads the entire CSV file
#'
#' @example inst/examples/load_persona.R
#'
#' @export
load_persona <- function(csv_path, name = NULL) {
  message(paste0("Loading persona '", name, "' from file '", csv_path, "'"))

  df <- read_persona_csv(csv_path)

  stopifnot(names(df)[1] == "index")

  if (is.null(name)) {
    if (ncol(df) > 2) {
      stop("Error: A name is required when the persona file contains >1 persona")
    }
    # There is only one persona in the file (after index)
    scores <- df[[2]]
  } else {
    # Select the column with the provided name
    scores <- df[[name]]
  }

  # Convert to named vector
  persona <- stats::setNames(scores, df[["index"]])

  assert_valid_persona(persona)

  return(persona)
}

#' Save Persona
#'
#' Save a persona by creating or appending to a CSV file.
#'
#' Saves a persona to a csv file. If the csv file already exists, the persona
#' will be appended as a new column, unless the provided name matches an existing
#' column, in which case it will overwrite the existing data.
#'
#' @param persona A named vector of integers representing the persona
#' @param csv_path Path to write a csv file, which may already exist
#' @param name A name for the persona
#' @param overwrite A boolean flag indicated whether it is permissible to overwrite
#' an existing personas with the same name
#'
#' @returns NULL
#'
#' @example inst/examples/save_persona.R
#'
#' @export
save_persona <- function(persona, csv_path, name, overwrite = FALSE) {
  if (name == "index") {
    message("Cannot name the persona 'index'. Persona not saved")
    return()
  }
  # TODO: add messages here, without polluting printed user info in shiny app
  assert_valid_persona(persona)

  # If file exists, we need to append the new persona carefully, making
  # sure the row names of the new persona are aligned with the 'index'
  # column of the dataframe
  if (file.exists(csv_path)) {
    df <- read_persona_csv(csv_path)

    # Check if we are overwriting an existing persona and delete the column if so
    # since otherwise we end up with `name1` and `name2` or something like that
    if (name %in% colnames(df)) {
      message(paste0("A persona with name '", name, "' already exists"))
      if (overwrite) {
        message("This will be overwritten with the new persona")
        df[[name]] <- NULL
      } else {
        message("Cannot overwrite existing persona. Please choose a different name")
        return()
      }
    }

    # Reorder persona to align it with the `index` column, then add
    # the reordered list to the data.frame as a new column
    df[[name]] <- persona[df[["index"]]]
  } else {
    # If file does *not* exist, simply crete a dataframe with two columns,
    # 'index' and '<name>'
    df <- data.frame(index = names(persona))
    df[[name]] <- persona
  }

  readr::write_csv(df, csv_path)
}

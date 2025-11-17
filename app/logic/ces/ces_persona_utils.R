# File:       utils.R
# Package:    biodt.recreation
# Repository: https://github.com/BioDT/uc-ces-recreation
# License:    MIT
# Copyright:  2025 BioDT and the UK Centre for Ecology & Hydrology
# Author(s):  Joe Marsh Rossney

box::use(
  utils[capture.output],
)

#' @export
get_persona_from_sliders <- function(input, layer_names) {
  sapply(
    layer_names,
    function(layer_name) input[[layer_name]],
    USE.NAMES = TRUE
  )
}

#' @export
update_user_info <- function(message, userInfoText) {
  clear_user_info(userInfoText)
  userInfoText(message)
}

#' Is Error
#'
#' Test if a value is an instance of `simpleError`.
#'
#' @param value The value to test.
#' @returns `TRUE` if the value is an error, `FALSE` otherwise.
#'
#' @keywords internal
#' @export
is_error <- function(value) inherits(value, "simpleError")

#' @export
clear_user_info <- function(userInfoText) userInfoText("")

#' Clean Error Message
#'
#' Extract clean error message by removing ANSI codes and progress text.
#' Looks for common error patterns that should be shown to users.
#'
#' @param message The raw message string that may contain ANSI codes and progress output.
#' @returns A cleaned message string.
#'
#' @keywords internal
#' @export
clean_error_message <- function(message) {
  # Remove ANSI color codes
  message <- gsub("\033\\[[0-9;]*m", "", message)

  # Remove progress bar patterns (everything before common error message starts)
  message <- gsub(
    ".*?(All the persona|Error:|The bounding box|cannot be|must be|at least)",
    "\\1",
    message,
    ignore.case = FALSE
  )

  # Trim whitespace
  message <- trimws(message)

  message
}

#' Capture Messages
#'
#' Modify a function so that, when executed, any messages that would
#' usually be printed to stdout are instead captured and held in a
#' variable. The wrapped function returns a pair `(result, message)`
#' that contains the original result and the captured messages.
#'
#' @param func A function that includes messages.
#' @returns The wrapped function.
#'
#' @keywords internal
#' @export
capture_messages <- function(func) {
  wrapped_func <- function(...) {
    message <- capture.output(
      result <- func(...),
      type = "message"
    )
    message <- paste(message, collapse = "\n") # split messages over lines
    return(list(result = result, message = message))
  }
  return(wrapped_func)
}

#' Errors as Messages
#'
#' Given a function that may throw an error, e.g. via `stop()`, produce
#' a function that instead prints the error message without crashing.
#'
#' This is achieved by wrapping the function execution in a `tryCatch`
#' and capturing any errors or warnings as a message.
#'
#' Note that in the case of an error being thrown, the function will
#' return the error. This can be checked by testing the return type, i.e.
#' `inherits(return_value$result, "simpleError")`, which will evaluate to
#' `TRUE` if an error was returned.
#'
#' @param func A function which can error out.
#' @returns The wrapped function.
#'
#' @keywords internal
#' @export
errors_as_messages <- function(func) {
  wrapped_func <- function(...) {
    result <- tryCatch(
      func(...),
      error = function(e) {
        message(conditionMessage(e))
        return(e)
      },
      warning = function(w) {
        message(conditionMessage(w))
      }
    )
    return(result)
  }
  return(wrapped_func)
}

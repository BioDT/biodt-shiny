# /app/logic/rtbm/utils.R

box::use(
  # No external packages needed for format_date_for_display currently
)

#' Format Date for Display
#'
#' Formats a date object or string into a user-friendly string (e.g., "Jan 01, 2023").
#' Handles various input types and potential parsing errors.
#'
#' @param date A Date object, or a string that can be parsed into a Date.
#' @return A formatted date string, or the original input if formatting fails.
#' @export
format_date_for_display <- function(date) {
  if (is.null(date)) {
    return("")
  }

  # Handle various date formats
  tryCatch(
    {
      # First check if it's already a Date object
      if (inherits(date, "Date")) {
        return(format(date, "%b %d, %Y"))
      }

      # Try to parse as ISO date
      parsed_date <- try(as.Date(date), silent = TRUE)
      if (!inherits(parsed_date, "try-error") && !is.na(parsed_date)) {
        return(format(parsed_date, "%b %d, %Y"))
      }

      # If all else fails, return as is
      return(as.character(date))
    },
    error = function(e) {
      # Fallback for any errors
      warning("Could not format date: ", date, " Error: ", e$message)
      return(as.character(date))
    }
  )
}

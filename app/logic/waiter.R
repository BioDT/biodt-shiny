box::use(
  shiny[tagList, h3],
  waiter[spin_loaders],
)

#' @export
waiter_text <- function(
    message = h3("Working..."),
    spinner = spin_loaders(
      id = 19,
      color = "#414f2f"
    )) {
  tagList(
    spinner,
    message
  )
}

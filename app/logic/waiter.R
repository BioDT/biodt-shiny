box::use(
  shiny[tagList, h3, tags],
  waiter[spin_loaders],
)

#' @export
waiter_text <- function(
    message = h3("Working..."),
    spinner = spin_loaders(
      id = 19,
      color = "#414f2f"
    )) {
  css <- tags$style(
    ".waiter-overlay-content{
      background-image: url('./img/forest2.png');
      background-repeat: no-repeat;
      background-size: 100%;
      background-position: bottom;
      display: flex;
    }"
  )

  tagList(
    spinner,
    message,
    css
  )
}

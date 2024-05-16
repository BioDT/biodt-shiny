box::use(
  shiny[tagList, h3, tags],
  waiter[spin_loaders],
)

#' @export
waiter_text <- function(
    message = h3("Working..."),
    style = "color: #414f2f;",
    css,
    spinner = spin_loaders(
      id = 19,
      color = "#414f2f"
    )) {
  if (missing(css)) { # default
    css <- tags$style(
      paste0(
        ".waiter-fullscreen {
          background-image: url('./img/forest2.png');
          background-repeat: no-repeat;
          background-size: 100%;
          background-position: bottom;
          display: flex; ",
        style,
        "}"
      )
    )
  } else { # if someone wants to completely rewrite css of the loader
    css <- css
  }

  tagList(
    css,
    spinner,
    message,
  )
}

box::use(
  shiny[updateSelectInput, updateRadioButtons],
)

#' @export
translate_multiple_choices <- function(
    session = session,
    which_type = c("select", "radio"),
    input_id = inputId,
    label,
    ...
    # TODO choices = c("createnew", "twolists", "namedlist", "factorslist", ?)
    # viz http://adv-r.had.co.nz/Functionals.html#functionals-fp or https://stackoverflow.com/questions/3505701/grouping-functions-tapply-by-aggregate-and-the-apply-family
    ) {
  updated_choices <- NULL

  if (which_type == "select") {
    updated_choices <- {
      updateSelectInput(
        session,
        input_id,
        label = i18n$t(label),
        choices = structure(
          lapply(..., identity()),
          names = list(lapply(names(...), i18n$t))
        )
      )
    }
  }
  updated_choices
}


test <- function(...) {
  print(...)

  res <- (...)
  res
}

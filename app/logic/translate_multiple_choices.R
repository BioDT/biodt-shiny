box::use(
  shiny[updateSelectInput, updateRadioButtons],
  shinyWidgets[updatePickerInput],
  tidyr[tibble, drop_na],
)

#' @export
translate_multiple_choices <- function(
    session = session,
    which_type = c("select", "radio", "picker"),
    input_id = inputId,
    label,
    inline = FALSE,
    i18n,
    choices_type = c("singlelist", "namedlist"),
    selected_choice = NULL,
    ...) {
  updated_choices <- NULL

  if (choices_type == "namedlist") {
    # on input there's named list ----
    if (which_type == "select") {
      updated_choices <- {
        updateSelectInput(
          session,
          input_id,
          label = i18n$t(label),
          choices = structure(
            lapply(..., identity),
            names = lapply(names(...), i18n$t)
          ),
          selected = selected_choice
        )
      }
    } else if (which_type == "radio") {
      updated_choices <- {
        updateRadioButtons(
          session,
          input_id,
          label = i18n$t(label),
          inline = inline,
          choices = structure(
            lapply(..., identity),
            names = lapply(names(...), i18n$t)
          ),
          selected = selected_choice
        )
      }
    } else if (which_type == "picker") {
      updated_choices <- {
        updatePickerInput(
          session,
          input_id,
          label = i18n$t(label),
          choices = structure(
            lapply(..., identity),
            names = lapply(names(...), i18n$t)
          ),
          selected = selected_choice
        )
      }
    } else {
      warning("Invalid type specified. Use 'select' (for selectInput) or 'radio' (for radioButtons) or 'picker' (for pickerInput).")
      return(NULL)
    }

    # on input there's single "vector" ----
  } else if (choices_type == "singlelist") {
    # on input there's a list of single items => going through few transformations...  ----
    tibbled_choices <- tibble(nms = (...), vals = (...))
    dropped_nas <- drop_na(tibbled_choices, vals)

    if (which_type == "radio") {
      updated_choices <- {
        updateRadioButtons(
          session,
          input_id,
          label = i18n$t(label),
          inline = inline,
          choices = structure(
            lapply(dropped_nas$vals, identity),
            names = lapply(dropped_nas$nms, i18n$t)
          ),
          selected = selected_choice
        )
      }
    } else if (which_type == "picker") {
      updated_choices <- {
        updatePickerInput(
          session,
          input_id,
          label = i18n$t(label),
          choices = structure(
            lapply(dropped_nas$vals, identity),
            names = lapply(dropped_nas$nms, i18n$t)
          ),
          selected = selected_choice
        )
      }
    } else if (which_type == "select") {
      updated_choices <- {
        updateSelectInput(
          session,
          input_id,
          label = i18n$t(label),
          choices = structure(
            lapply(dropped_nas$vals, identity),
            names = lapply(dropped_nas$nms, i18n$t)
          ),
          selected = selected_choice
        )
      }
    } else {
      warning("Invalid type specified. Use 'select' (for selectInput) or 'radio' (for radioButtons) or 'picker' (for pickerInput).")
      return(NULL)
    }
  }
  updated_choices
}

box::use(
  shiny,
)

#' @export
update_selector_personas <- function(
  session,
  df_personas,
  selector_id = "persona_selector",
  selected_persona = NULL
) {
  list_personas <- names(df_personas) |>
    setdiff("index")

  if (is.null(selected_persona)) {
    selected_persona <- list_personas[1]
  }

  shiny$updateSelectInput(
    session,
    selector_id,
    choices = c(
      list_personas
    ),
    selected = selected_persona
  )
}

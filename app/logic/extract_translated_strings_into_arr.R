box::use(
  jsonlite[fromJSON],
  data.table[as.data.table], # should've used eg. https://www.appsilon.com/post/r-dtplyr
  dplyr[filter, glimpse],
  config,
)

#' @export
extract_translated_ass_array <- function(en_string) {
  filepath_with_translations <- config$get("transl_file")

  dframe_w_tnsl <- fromJSON(filepath_with_translations)

  dtabl_w_tnsl <- as.data.table(dframe_w_tnsl$translation)

  chrs_arr_tnslated <- dtabl_w_tnsl |>
    dplyr::filter(en == en_string) |>
    c()

  chrs_arr_tnslated
}

#' Calculate diff with leading NA
#' @noRd
diffna <- function(x) {
  x - shift(x, 1)
}

#' Collapse character vector with clean presentation
#' @noRd
glue_collapse_vec <- function(x) {
  glue::glue_collapse(x, sep = ", ", last = " and ")
}

#' Split character vector and return list
#' @noRd
split_words <- function(x) {
  w <- strsplit(x, split = " ", fixed = TRUE)
  list(sapply(w, "[", 1), sapply(w, "[", 2))
}

#' @keywords internal
#' @rawNamespace import(rlang, except = ":=")
#' @import vctrs
#' @import data.table
#' @importFrom glue glue glue_collapse

"_PACKAGE"

utils::globalVariables(
  c("time", "time_start", "time_end",
    "entity", "entity_start", "entity_end",
    "x_start", "x_end",
    "y_start", "y_end")
)

NULL

#' Test if the object is an action vector
#'
#' This function returns `TRUE` for action vectors and `FALSE` for all
#' other objects.
#'
#' @param x An object.
#'
#' @return `TRUE` if the object inherits from the `action` class.
#' @export
is_action <- function(x) {
  inherits(x, "action")
}

#' Coerce tracking object to an action vector
#'
#' @param event An event data.table. See `as_event()`.
#'
#' @return A vector of actions.
#' @export
#'
#' @examples
#' data <- data.frame(period = 1, team = "home",
#'                    entity_start = "player1", entity_end = "player3",
#'                    time_start = 9.2, time_end = 11.1,
#'                    x_start = 33.2, x_end = 49.2,
#'                    y_start = 60.5, y_end = 67.1,
#'                    type = "pass", subtype = NA)
#' event <- as_event(data)
#' as_action(event)
as_action <- function(event) {
  UseMethod("as_action")
}

#' @export
#' @rdname as_action
as_action.default <- function(event) {
  event <- as_event(event)
  as_action(event = event)
}

#' @export
#' @rdname as_action
as_action.event <- function(event) {

  if (!is_event(event)) {
    abort("`event` must be an event object. Try `as_event(data)`.")
  }

  action <- new_action(
    entity_start = sapply(event$entity, "[[", 1),
    entity_end   = sapply(event$entity, "[[", 2),
    team         = event$team,
    time_start   = sapply(event$time,   "[[", 1),
    time_end     = sapply(event$time,   "[[", 2),
    x_start      = sapply(event$x,      "[[", 1),
    x_end        = sapply(event$x,      "[[", 2),
    y_start      = sapply(event$y,      "[[", 1),
    y_end        = sapply(event$y,      "[[", 2),
    type         = event$type,
    subtype      = event$subtype
  )

  validate_action(action)

}

new_action <- function(entity_start, entity_end, team,
                       time_start = numeric(), time_end = numeric(),
                       x_start = numeric(), x_end = numeric(),
                       y_start = numeric(), y_end = numeric(),
                       type, subtype) {

  flds <- vec_recycle_common(
    entity_start = entity_start,
    entity_end   = entity_end,
    team         = team,
    time_start   = time_start,
    time_end     = time_end,
    x_start      = x_start,
    x_end        = x_end,
    y_start      = y_start,
    y_end        = y_end,
    type         = type,
    subtype      = subtype
  )

  action <- new_rcrd(
    fields = df_list(
      entity_start = flds$entity_start,
      entity_end   = flds$entity_end,
      team         = flds$team,
      time_start   = flds$time_start,
      time_end     = flds$time_end,
      x_start      = flds$x_start,
      x_end        = flds$x_end,
      y_start      = flds$y_start,
      y_end        = flds$y_end,
      type         = flds$type,
      subtype      = flds$subtype
    ),
    class = "action"
  )

  validate_action(action)

}

validate_action <- function(x) {
  vec_assert(
    identical(
      fields(x), c("entity_start", "entity_end", "team",
                   "time_start", "time_end", "x_start", "x_end",
                   "y_start", "y_end", "type", "subtype")
    )
  )
  # vec_assert(field(x, "time"), numeric())
  # vec_assert(field(x, "x"),    numeric())
  # vec_assert(field(x, "y"),    numeric())
  x
}

#' @export
format.action <- function(x, ...) {

  # act <- as_action(evt)

  z <- vec_data(x)
  t <- pillar::style_subtle("[t]")
  e <- pillar::style_subtle("[e]")
  l <- pillar::style_subtle("[l]")
  t <- pillar::style_subtle("[t]")

  format(
    sprintf(
      "%s %1.2f %s %s <%s> %s %1.1f, %1.1f %s %s",
      t, z$time_start,
      e, z$entity_start,z$team,
      l, z$x_start, z$y_start,
      t, z$type
    )
  )

}

#' @export
print.action <- function(x, ..., n = NULL) {
  obj_print_header(x)
  obj_print_data(x, n = n)
  obj_print_footer(x, n = n)
}

#' @export
obj_print_data.action <- function(x, n = NULL) {
  x <- utils::head(x, n %||% 10)
  cat(format(x), sep = "\n")
}

#' @export
obj_print_footer.action <- function(x, n = NULL) {

  if (vec_size(x) - n %||% 10 > 0) {
    cat(
      pillar::style_subtle(
        sprintf(
          "%s %d %s",
          "... with", vec_size(x) - n %||% 10, "more actions"
        )
      )
    )
  } else {
    invisible(x)
  }

}

#' @export
vec_ptype_full.action <- function(x) {
  "action"
}

#' @export
vec_ptype_abbr.action <- function(x) {
  "actn"
}


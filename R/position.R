#' Test if the object is a position vector
#'
#' This function returns `TRUE` for position vectors and `FALSE` for all
#' other objects.
#'
#' @param x An object.
#'
#' @return `TRUE` if the object inherits from the `position` class.
#' @export
is_position <- function(x) {
  inherits(x, "position")
}

#' Coerce tracking object to a position vector
#'
#' @param tracking A tracking data.table. See `as_tracking()`.
#' @param ball Include ball position? If `FALSE`, ball is not included.
#' @param expand Expand tracking data to include all (entity, time)
#' combinations.
#'
#' @return A vector of positions.
#' @export
#'
#' @examples
#' data <- data.frame(period = 1, time = 0.04, team = "home",
#'                    entity = "player1", x = 3.62, y = 21.1)
#' tracking <- as_tracking(data)
#' as_position(tracking)
as_position <- function(tracking, ball = TRUE, expand = FALSE) {
  UseMethod("as_position")
}

#' @export
#' @rdname as_position
as_position.default <- function(tracking, ball = TRUE, expand = FALSE) {
  tracking <- as_tracking(tracking)
  as_position(tracking = tracking, ball = ball, expand = expand)
}

#' @export
#' @rdname as_position
as_position.tracking <- function(tracking, ball = TRUE, expand = FALSE) {


  if (!is_tracking(tracking)) {
    abort("`tracking` must be a tracking object. Try `as_tracking(data)`.")
  }

  if (!ball) {
    tracking <- tracking[get_entity(tracking) != "ball"]
  }

  if (expand) {
    tracking <-
      tracking[
        CJ(entity = paste(tracking$entity, tracking$team),
           time = tracking$time,
           unique = TRUE,
           sorted = TRUE)[, c("entity", "team") := split_words(entity)],
        on = c("entity", "team", "time")
      ]
  }

  position <- new_position(
    entity = tracking$entity,
    team   = tracking$team,
    time   = tracking$time,
    x      = tracking$x,
    y      = tracking$y
  )

  validate_position(position)

}

new_position <- function(entity, team,
                         time = numeric(), x = numeric(), y = numeric()) {

  z <- vec_recycle_common(
    entity = entity, team = team, time = time, x = x, y = y
  )

  position <- new_rcrd(
    fields = df_list(
      entity = z$entity,
      team   = z$team,
      time   = as.numeric(z$time),
      x      = as.numeric(z$x),
      y      = as.numeric(z$y)
    ),
    class = "position"
  )

  validate_position(position)

}

validate_position <- function(x) {
  vec_assert(
    identical(
      fields(x), c("entity", "team", "time", "x", "y")
    )
  )
  vec_assert(field(x, "time"), numeric())
  vec_assert(field(x, "x"),    numeric())
  vec_assert(field(x, "y"),    numeric())
  x
}

#' Build a position vector
#'
#' @param entity a vector of entities, e.g. "player1".
#' @param team a vector denoting team name, e.g. "Liverpool".
#' @param time a numeric vector of times.
#' @param x a numeric vector of x coordinates.
#' @param y a numeric vector of y coordinates.
#'
#' @return A position vector.
#' @export
#'
#' @examples
#' position(entity = "Mount", team = "Chelsea", time = 30.4, x = 70, y = 32)
position <- function(entity, team,
                     time = double(), x = double(), y = double()) {
  new_position(entity, team, time, x, y)
}

#' @export
format.position <- function(x, ...) {

  z  <- vec_data(x) # \U26BD
  t  <- pillar::style_subtle("t:")
  e  <- pillar::style_subtle("e:")
  xy <- pillar::style_subtle("xy:")

  format(
    sprintf(
      "%s %1.2f %s %s <%s> %s %1.1f, %1.1f",
      t, z$time, e, z$entity, z$team, xy, z$x, z$y
    )
  )

}

#' @export
print.position <- function(x, ..., n = NULL) {
  obj_print_header(x)
  obj_print_data(x, n = n)
  obj_print_footer(x, n = n)
}

#' @export
obj_print_data.position <- function(x, n = NULL) {
  x <- utils::head(x, n %||% 10)
  cat(format(x), sep = "\n")
}

#' @export
obj_print_footer.position <- function(x, n = NULL) {

  if (vec_size(x) - n %||% 10 > 0) {
    cat(
      pillar::style_subtle(
        sprintf(
          "%s %d %s",
          "... with", vec_size(x) - n %||% 10, "more positions"
        )
      )
    )
  } else {
    invisible(x)
  }

}

#' @export
vec_ptype_full.position <- function(x) {
  "position"
}

#' @export
vec_ptype_abbr.position <- function(x) {
  "posn"
}



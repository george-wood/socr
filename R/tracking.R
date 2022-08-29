#' Test if the object is a tracking data.table
#'
#' This function returns `TRUE` for tracking data.tables and `FALSE` for all
#' other objects.
#'
#' @param data An object.
#'
#' @return `TRUE` if the object inherits both `tracking` and
#' `data.table` classes.
#' @export
is_tracking <- function(data) {
  inherits_all(data, c("tracking", "data.table"))
}

#' Coerce lists, matrices, and data frames to tracking data.tables
#'
#' @param data A data frame, list, matrix, or other object that could be
#' coerced to a `data.table`.
#' @param period Name of column denoting the period.
#' @param team Name of column denoting the team.
#' @param entity Name of column denoting entities, i.e. players and the ball.
#' If your tracking data includes the ball, it should be denoted by the value
#' "ball".
#' @param time Name of column denoting time.
#' @param x Name of column denoting position in x dimension.
#' @param y Name of column denoting position in y dimension.
#' @param nafill The method for dealing with NA in time, x, and y columns.
#' See `data.table::nafill`
#' @param ... Unused, for extensibility.
#'
#' @return A tracking data.table
#' @export
#'
#' @examples
#' data <- data.frame(period = 1, team = "home", entity = "player1",
#'                    time = 6.2, x = 3.62, y = 21.1)
#' as_tracking(data)
as_tracking <- function(data, ...) {
  UseMethod("as_tracking")
}

#' @export
#' @rdname as_tracking
as_tracking.default <- function(data, ...) {
  as_tracking(as.data.frame(data), ...)
}

#' @export
#' @rdname as_tracking
as_tracking.data.frame <-
  function(data,
           period = "period",
           team = "team",
           entity = "entity",
           time = "time",
           x = "x",
           y = "y",
           nafill = NULL,
           ...) {

  # Construct data.table object
  #
  # For `as_tracking()`, `data` is copied, converted to a list, then
  # reassembled as a data.table
  data <- unclass(copy(data))

  if (!is.list(data)) {
    abort("`data` must be coercible to a list.")
  }

  setDT(data)

  # Check for required columns and ball
  required <- c(period, team, entity, time, x, y) %in% names(data)
  output   <- c("period", "team", "entity", "time", "x", "y")

  if (!any(required)) {
    abort(glue("Column not found: ", glue_collapse_vec(output[!required])))
  }

  if (!"ball" %in% data[, entity]) {
    warn("`entity` should contain the value 'ball'.")
  }

  # Mutate features
  data <- data[
    , c(output) :=
      list(
        as.integer(period),
        as.character(team),
        as.character(entity),
        as.double(time),
        as.double(x),
        as.double(y)
      ),
    env = list(
      period = period,
      team   = team,
      entity = entity,
      time   = time,
      x      = x,
      y      = y
    )
  ][
    , c(output, setdiff(names(data), output)), with = FALSE
  ][
    entity == "ball", team := "ball"
  ][]

  setnafill(
    x = data, type = nafill %||% "const", fill = NA,
    cols = c("time", "x", "y")
  )

  # Set tracking class, key, and validate the return object
  setattr(
    data, name = "class", value = c("tracking", class(data))
  )

  setkey(
    data, entity, team, time
  )

  validate_tracking(data)

}

tracking <- function(period, team, entity, time, x, y) {

  data <-
    data.table(
      period = as.integer(period),
      team   = as.character(team),
      entity = as.character(entity),
      time   = as.double(time),
      x      = as.double(x),
      y      = as.double(y)
    )

  # Set tracking class, key, and validate the return object
  setattr(
    data, name = "class", value = c("tracking", class(data))
  )

  setkey(
    data, entity, team, time
  )

  validate_tracking(data)

}

validate_tracking <- function(x) {
  vec_assert(field(x, "period"), integer())
  vec_assert(field(x, "entity"), character())
  vec_assert(field(x, "team"),   character())
  vec_assert(field(x, "time"),   double())
  vec_assert(field(x, "x"),      double())
  vec_assert(field(x, "y"),      double())

  if (!identical(key(x), c("entity", "team", "time"))) {
    setkeyv(x, cols = c("entity", "team", "time"))
  }

  x
}



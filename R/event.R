#' Test if the object is an event data.table
#'
#' This function returns `TRUE` for event data.tables and `FALSE` for all
#' other objects.
#'
#' @param data An object.
#'
#' @return `TRUE` if the object inherits both `event` and
#' `data.table` classes.
#' @export
is_event <- function(data) {
  inherits_all(data, c("event", "data.table"))
}

#' Coerce lists, matrices, and data frames to event data.tables
#'
#' @param data A data frame, list, matrix, or other object that could be
#' coerced to a `data.table`.
#' @param period A character vector denoting the period.
#' @param team A character vector denoting the team.
#' @param entity A character vector of variables denoting the entity
#' that started and ended an event.
#' @param time A character vector of variables denoting the times that
#' an event started and ended.
#' @param x A character vector of variables denoting the location that
#' an event started and ended in the x dimension.
#' @param x A character vector of variables denoting the location that
#' an event started and ended in the y dimension.
#' @param type A character vector denoting the event type.
#' @param subtype A character vector denoting the event subtype.
#' @param ... Unused, for extensibility.
#'
#' @return An event data.table
#' @export
#'
#' @examples
#' data <- data.frame(period = 1, team = "home",
#'                    entity_start = "player1", entity_end = "player3",
#'                    time_start = 9.2, time_end = 11.1,
#'                    x_start = 33.2, x_end = 49.2,
#'                    y_start = 60.5, y_end = 67.1,
#'                    type = "pass", subtype = NA)
#' as_event(data)
#' @export
as_event <- function(data, ...) {
  UseMethod("as_event")
}

#' @export
#' @rdname as_event
as_event.default <- function(data,  ...) {
  as_event(as.data.frame(data), ...)
}

#' @export
#' @rdname as_event
as_event.data.frame <-
  function(data,
           period = "period",
           team = "team",
           entity = c("entity_start", "entity_end"),
           time = c("time_start", "time_end"),
           x = c("x_start", "x_end"),
           y = c("y_start", "y_end"),
           type = "type",
           subtype = "subtype",
           ...) {

    # Construct data.table object
    #
    # For `as_tracking()`, `data` is copied, converted to a list, then
    # reassembled as a data.table
    data <- unclass(data)

    if (!is.list(data)) {
      abort("`data` must be coercible to a list.")
    }

    setDT(data)

    # Check for required columns and ball
    required <- c(period, team, entity, time, x, y,
                  type, subtype) %in% names(data)
    output   <- c("period", "team", "entity_start", "entity_end",
                  "time_start", "time_end", "x_start", "x_end",
                  "y_start", "y_end", "type", "subtype")

    if (!any(required)) {
      abort(glue("Column not found: ", glue_collapse_vec(output[!required])))
    }

    # Mutate features
    data <- data[
      , c(output) :=
        list(
          as.integer(period),
          as.character(team),
          as.character(entity_start),
          as.character(entity_end),
          as.double(time_start),
          as.double(time_end),
          as.double(x_start),
          as.double(x_end),
          as.double(y_start),
          as.double(y_end),
          as.character(type),
          as.character(subtype)
        ),
      env = list(
        period       = period,
        team         = team,
        entity_start = entity[1],
        entity_end   = entity[2],
        time_start   = time[1],
        time_end     = time[2],
        x_start      = x[1],
        x_end        = x[2],
        y_start      = y[1],
        y_end        = y[2],
        type         = type,
        subtype      = subtype
      )
    ][
      , c(output, setdiff(names(data), output)), with = FALSE
    ]

    # Set tracking class, key, and validate the return object
    setattr(
      data, name = "class", value = c("entity", class(data))
    )

    setkey(
      data, entity_start, entity_end, team, time_start, time_end
    )

    validate_event(data)

  }

event <- function(period, team, entity_start, entity_end,
                  time_start, time_end, x_start, x_end,
                  y_start, y_end, type, subtype) {

  data <-
    data.table(
      period       = as.integer(period),
      team         = as.character(team),
      entity_start = as.character(entity_start),
      entity_end   = as.character(entity_end),
      time_start   = as.double(time_start),
      time_end     = as.double(time_end),
      x_start      = as.double(x_start),
      x_end        = as.double(x_end),
      y_start      = as.double(y_start),
      y_end        = as.double(y_end),
      type         = as.character(type),
      subtype      = as.character(subtype)
    )

  # Set tracking class, key, and validate the return object
  setattr(
    data, name = "class", value = c("event", class(data))
  )

  setkey(
    data, time_start, time_end
  )

  validate_event(data)

}

validate_event <- function(x) {
  vec_assert(field(x, "period"),       integer())
  vec_assert(field(x, "entity_start"), character())
  vec_assert(field(x, "entity_end"),   character())
  vec_assert(field(x, "time_start"),   double())
  vec_assert(field(x, "time_end"),     double())
  vec_assert(field(x, "x_start"),      double())
  vec_assert(field(x, "x_end"),        double())
  vec_assert(field(x, "y_start"),      double())
  vec_assert(field(x, "y_end"),        double())
  vec_assert(field(x, "type"),         character())
  vec_assert(field(x, "subtype"),      character())

  if (!identical(key(x), c("entity_start", "entity_end", "team",
                           "time_start", "time_end"))) {
    setkeyv(x, cols = c("entity_start", "entity_end",
                        "team", "time_start", "time_end"))
  }

  x
}


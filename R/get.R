#' Get entities from a position object
#'
#' @param x A position object.
#' @param team Character vector. Name of a team in `x`.
#'
#' @return A character vector of entities.
#' @export
#'
#' @examples
#' x <- position(entity = "Mount", team = "Chelsea",
#'               time = 1, x = 5, y = 6)
#' get_entity(x)
get_entity <- function(x, team) {
  UseMethod("get_entity")
}

#' @export
get_entity.position <- function(x, team) {

  if (!missing(team)) {
    if (!team %in% field(x, "team")) {
      abort("`team` not found.")
    }
    x <- x[field(x, "team") %in% team]
  }

  field(x, "entity")

}

#' @export
get_entity.tracking <- function(x, team) {

  if (!missing(team)) {
    id <- team
    if (!id %in% field(x, "team")) {
      abort("`team` not found.")
    }
    x <- x[team %in% id]
  }

  x$entity

}


#' Get location from a position object
#'
#' @param x A position object.
#' @param entity Character vector denoting entities in `x`.
#'
#' @return A matrix of locations with columns "x" and "y".
#' @export
#'
#' @examples
#' x <- position(entity = "Mount", team = "Chelsea",
#'               time = 1, x = 5, y = 6)
#' get_location(x)
get_location <- function(x, entity) {
  UseMethod("get_location")
}

#' @export
get_location.position <- function(x, entity) {

  if (!missing(entity)) {
    x <- x[field(x, "entity") %in% entity]
  }

  matrix(data = c(field(x, "x"), field(x, "y")),
         ncol = 2,
         dimnames = list(field(x, "entity"), c("x", "y")))

}

#' @export
get_location.tracking <- function(x, entity) {

  if (!missing(entity)) {
    x <- x[field(x, "entity") %in% entity]
  }

  as.matrix(x = x[, c("x", "y", "entity")],
            rownames = "entity")

}


#' Get team from a position object
#'
#' @param x A position object.
#'
#' @return A character vector of teams.
#' @export
#'
#' @examples
#' x <- position(entity = "Mount", team = "Chelsea",
#'               time = 1, x = 5, y = 6)
#' get_team(x)
get_team <- function(x) {
  UseMethod("get_team")
}

#' @export
get_team.position <- function(x) {
  # matrix(data = field(x, "team"),
  #        dimnames = list(field(x, "entity"), "team"))
  field(x, "team")
}

#' @export
get_team.tracking <- function(x) {
  # as.matrix(x = x[, c("entity", "team")],
  #           rownames = "entity")
  x$team
}


#' Get time from a position object
#'
#' @param x A position object.
#'
#' @return A numeric vector of times.
#' @export
#'
#' @examples
#' x <- position(entity = "Mount", team = "Chelsea",
#'               time = 1, x = 5, y = 6)
#' get_time(x)
get_time <- function(x) {
  UseMethod("get_time")
}

#' @export
get_time.position <- function(x) {
  # matrix(data = field(x, "time"),
  #        dimnames = list(field(x, "entity"), "time"))
  field(x, "time")
}

#' @export
get_time.tracking <- function(x) {
  # as.matrix(x = x[, c("entity", "time")],
  #           rownames = "entity")
  x$time
}

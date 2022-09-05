#' Calculate displacement between positions
#'
#' @param p A position vector.
#' @param from A vector of entity names, e.g. c("player1", "player2").
#' @param to A vector of entity names, e.g. c("player1", "player2").
#'
#' @return If `from = NULL` and `to = NULL`, a list. If `from` or `to` is an
#' an entity, a matrix with dimension `length(from)` * `length(to)`.
#'
#' @seealso [distance()]
#'
#' @export
#' @examples
#' p <- position(entity = c("Havertz", "Mount"), team = "Chelsea",
#'               time = 10, x = c(15, 10), y = c(60, 50))
#' displacement(p, from = "Havertz", to = "Mount")
displacement <- function(p, from = NULL, to = NULL) {
  UseMethod("displacement")
}

#' @export
#' @rdname displacement
displacement.position <- function(p, from = NULL, to = NULL) {

  if (!is_position(p)) {
    abort("`p` must be a position vector.")
  }

  if (is.null(from) & is.null(to)) {

    s <- split.data.frame(get_location(p), get_entity(p))
    d <- do.call(rbind, lapply(s, function(x) apply(x, 2, diffna)))
    cbind(d, "xy" = sqrt(d[, "x"]**2 + d[, "y"]**2))

  } else {

    from <- from %||% vec_unique(get_entity(p))
    to   <- to   %||% vec_unique(get_entity(p))

    if (any(!to %in% vec_unique(get_entity(p)))) {
      abort("Invalid entity named in `to`.")
    }

    if (any(!from %in% vec_unique(get_entity(p)))) {
      abort("Invalid entity named in `from`.")
    }

    p <- p[get_entity(p) %in% c(from, to)]

    s <- split.data.frame(
      x = get_location(p),
      f = rep(vec_unique(get_time(p)), vec_unique_count(get_entity(p)))
    )

    # distance in xy
    xy <-
      simplify2array(
        lapply(
          s,
          function(x) as.matrix(stats::dist(x))[from, to, drop = FALSE]
        )
      )

    # x, y displacement components
    self <- identical(from, to) & vec_size(from) == 1

    z <- lapply(
      list("x", "y"),
      function(x)
        simplify2array(
          lapply(
            s, function(z) {
              if (self)
                outer(z[, x], z[, x], `-`)
              else
                outer(z[, x], z[, x], `-`)[from, to, drop = FALSE]
            }
          )
        )
    )

    # result as array
    array(
      data = c(z[[1]], z[[2]], xy),
      dim = c(vec_size(from), vec_size(to), vec_unique_count(get_time(p)), 3),
      dimnames = list(from, to, vec_unique(get_time(p)), c("x", "y", "xy"))
    )

  }
}


#' Calculate distance between positions
#'
#' @param p A position vector.
#' @param from A vector of entity names, e.g. c("player1", "player2").
#' @param to A vector of entity names, e.g. c("player1", "player2").
#'
#' @return If `from = NULL` and `to = NULL`, a list. If `from` or `to` is an
#' an entity, a matrix with dimension `length(from)` * `length(to)`.
#'
#' @seealso [displacement()]
distance <- function(p, from = NULL, to = NULL) {
  UseMethod("distance")
}

#' @export
#' @rdname distance
distance.position <- function(p, from = NULL, to = NULL) {

  if (!is_position(p)) {
    abort("`p` must be a position vector.")
  }

  if (any(!to %in% vec_unique(get_entity(p)))) {
    abort("Invalid entity named in `to`.")
  }

  abs(displacement(p, from = from, to = to))

}


#' Calculate velocity of an entity
#'
#' @param p A position vector.
#'
#' @return A numeric vector of `length(p)`.
velocity <- function(p) {
  UseMethod("velocity")
}

#' @export
#' @rdname velocity
velocity.position <- function(p) {

  if (!is_position(p)) {
    abort("`p` must be a position vector.")
  }

  s  <- split(get_time(p), get_entity(p))
  dt <- utils::stack(sapply(s, diffna, simplify = FALSE))

  if (any(dt == 0, na.rm = TRUE)) {
    abort("Velocity is undefined when `t` is zero.")
  }

  displacement(p) / dt$value

}

#' Calculate speed of an entity
#'
#' @param p A position vector.
#'
#' @return A numeric vector of `length(p)`
speed <- function(p) {
  UseMethod("speed")
}

#' @export
#' @rdname speed
speed.position <- function(p) {

  if (!is_position(p)) {
    abort("`p` must be a position vector.")
  }

  s  <- split(get_time(p), get_entity(p))
  dt <- utils::stack(sapply(s, diffna, simplify = FALSE))

  if (any(dt == 0, na.rm = TRUE)) {
    abort("Speed is undefined when `t` is zero.")
  }

  distance(p) / dt$values

}

#' Calculate angle of an entity
#'
#' This function calculates the angle of an entity's movement relative to the
#' x-axis
#'
#' @param p A position vector.
#'
#' @return A numeric vector of `length(p)`
theta <- function(p) {
  UseMethod("theta")
}

#' @export
#' @rdname theta
theta.position <- function(p) {

  if (!is_position(p)) {
    abort("`p` must be a position vector.")
  }

  v <- velocity(p)
  calculate_theta(v[, "x"], v[, "y"])
}

calculate_theta <- function(vx, vy) {
  acos(vx / sqrt(vx**2 + vy**2))
}

#' Calculate speed of an entity as ratio of maximum speed
#'
#' This function calculates the ratio between an entity's speed and a
#' fixed maximum speed
#'
#' @param p A position vector.
#' @param max_speed Assumed maximum speed.
#'
#' @return A numeric vector of `length(p)`
speed_ratio <- function(p, max_speed = 13) {

  if (!is_position(p)) {
    abort("`p` must be a position vector.")
  }

  s <- speed(p)
  (s[, "xy"] / 13)**2

}

#' Calculate expected location of an entity
#'
#' @param p A position vector.
#
#' @return An array with `length(p)` rows and columns "x" and "y".
expected_location <- function(p) {

  if (!is_position(p)) {
    abort("`p` must be a position vector.")
  }

  get_location(p) + velocity(p)[, c("x", "y")] / 2
}

#' Calculate influence radius of a player
#'
#' This function calculates the influence radius of player(s) given their
#' distance to the ball.
#'
#' @param p A position vector.
#
#' @return A numeric vector of `length(p)`
influence_radius <- function(p) {

  if (!is_position(p)) {
    abort("`p` must be a position vector.")
  }

  players <- setdiff(get_entity(p), "ball")
  d <- distance(p, from = players, to = "ball")[, , , "xy"]
  ir <- pmin(4 + ((d^3) / ((18^3) / 6)), 10)

  # return named vector
  nm <- rep(row.names(ir), each = ncol(ir))
  ir <- as.numeric(t(ir))
  names(ir) <- nm
  ir

}


#' Model pitch control
#'
#' @param tracking A tracking data.table. See `as_tracking()`.
#' @param grid A numeric vector giving grid dimensions, e.g. c(100, 50)
#' @param cells Number of cells at which to evaluate pitch control.
#'
#' @return A data.table with four columns giving the pitch control for each
#' (time, x, y) tuple in `tracking`.
#'
#' @export
pitch_control <- function(tracking, grid = c(120, 80), cells = 200) {

  validate_tracking(tracking)

  if (vec_unique_count(tracking$time) > 1e4) {
    abort("`pitch_control()` does not scale to long periods of data.")
  }

  players <- as_position(tracking, ball = FALSE, expand = TRUE)
  team_id <- vec_group_loc(get_team(players))

  # metrics
  lo <- lapply(asplit(get_location(players), 1), as.numeric)
  mu <- asplit(expected_position(players), 1)
  sr <- speed_ratio(players)
  ir <- influence_radius(as_position(tracking, expand = TRUE))

  # rotation and scaling matrices
  rotation <- lapply(
    theta(players),
    function(x) matrix(c(cos(x), sin(x), -sin(x), cos(x)), nrow = 2)
  )

  scaling <- purrr::pmap( # t(ir)
    list(as.numeric((1 + c(sr)) * c(ir) / 2),
         as.numeric((1 - c(sr)) * c(ir) / 2)),
    function(x, y) matrix(c(x, 0, 0, y), nrow = 2)
  )

  # sigma
  sigma <- purrr::pmap(
    list(rotation, scaling),
    function(x, y) x %*% y %*% y %*% solve(x)
  )

  # pitch grid
  pitch <- expand.grid(
    x = seq(0, grid[1], length.out = cells),
    y = seq(0, grid[2], length.out = cells)
  )

  # influence by player for each pitch cell
  pitch_influence <- purrr::pmap(
    list(mu, sigma),
    function(x, y) mvtnorm::dmvnorm(pitch, x, y)
  )

  location_influence <- purrr::pmap_dbl(
    list(lo, mu, sigma),
    function(x, y, z) mvtnorm::dmvnorm(x, y, z)
  )

  influence <- do.call(cbind, pitch_influence) %*%
    Matrix::Diagonal(x = 1 / location_influence)

  # multiply by -1 for summation convenience
  influence[, team_id[[1, 2]]] <- influence[, team_id[[1, 2]]] * -1
  summation <- rowsum(t(as.matrix(influence)), get_time(players), na.rm = TRUE)
  pc <- 1 / (1 + exp(summation))

  # return
  res <- data.table(
    time = rep(vec_unique(get_time(tracking)), each = vec_size(pitch)),
    pitch,
    control = c(t(pc))
  )

  res[time != min(time) & stats::complete.cases(res)]

}


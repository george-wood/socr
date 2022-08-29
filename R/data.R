#' Sample tracking data from Metrica Sports
#'
#' Tracking data for sample game 1.
#'
#' @format A data frame with 1,639,165 rows and 6 variables:
#' \describe{
#'   \item{period}{Period of the match}
#'   \item{time}{Time of match, measured in seconds}
#'   \item{entity}{Entity, i.e. a player or the ball}
#'   \item{x}{X position of the entity}
#'   \item{y}{Y position of the entity}
#'   \item{team}{Name of a team. For the ball entity, the team is "ball".}
#' }
#' @source {https://github.com/metrica-sports/sample-data}
"metrica_tracking"

#' Sample event data from Metrica Sports
#'
#' Event data for sample game 1.
#'
#' @format A data frame with 892 rows and 12 variables:
#' \describe{
#'   \item{team}{Name of a team}
#'   \item{type}{Event type}
#'   \item{subtype}{Event subtype}
#'   \item{period}{Period of the match}
#'   \item{time_start}{Time at start of event}
#'   \item{time_end}{Time at end of event}
#'   \item{entity_start}{Entity starting the event}
#'   \item{entity_end}{Entity ending the event}
#'   \item{x_start}{X position at start of event}
#'   \item{x_end}{X position at end of event}
#'   \item{y_start}{Y position at start of event}
#'   \item{y_end}{Y position at end of event}
#' }
#' @source {https://github.com/metrica-sports/sample-data}
"metrica_event"

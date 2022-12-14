% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tracking.R
\name{as_tracking}
\alias{as_tracking}
\alias{as_tracking.default}
\alias{as_tracking.data.frame}
\title{Coerce lists, matrices, and data frames to tracking data.tables}
\usage{
as_tracking(data, ...)

\method{as_tracking}{default}(data, ...)

\method{as_tracking}{data.frame}(
  data,
  period = "period",
  team = "team",
  entity = "entity",
  time = "time",
  x = "x",
  y = "y",
  nafill = NULL,
  ...
)
}
\arguments{
\item{data}{A data frame, list, matrix, or other object that could be
coerced to a \code{data.table}.}

\item{...}{Unused, for extensibility.}

\item{period}{Name of column denoting the period.}

\item{team}{Name of column denoting the team.}

\item{entity}{Name of column denoting entities, i.e. players and the ball.
If your tracking data includes the ball, it should be denoted by the value
"ball".}

\item{time}{Name of column denoting time.}

\item{x}{Name of column denoting position in x dimension.}

\item{y}{Name of column denoting position in y dimension.}

\item{nafill}{The method for dealing with NA in time, x, and y columns.
See \code{data.table::nafill}}
}
\value{
A tracking data.table
}
\description{
Coerce lists, matrices, and data frames to tracking data.tables
}
\examples{
data <- data.frame(period = 1, team = "home", entity = "player1",
                   time = 6.2, x = 3.62, y = 21.1)
as_tracking(data)
}

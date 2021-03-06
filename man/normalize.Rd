% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{normalize}
\alias{normalize}
\title{Normalize}
\usage{
normalize(x, proportion = 1)
}
\arguments{
\item{x}{A numeric vector}

\item{proportion}{A number between 0 and 1}
}
\value{
A numeric vector of the normalized values.
}
\description{
Normalizes the values in x so that they sum to 1.
Proportion indicates whether to set values to 0.
}
\examples{
normalize(c(0.1, 100, 10), proportion = 0.95)
}

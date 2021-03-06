% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/complete.R
\name{dts_complete}
\alias{dts_complete}
\title{Completes a data time series}
\usage{
dts_complete(
  x,
  dtt = "DateTime",
  colname = dts_colnames(x),
  from = min(x[[dtt]]),
  to = max(x[[dtt]]),
  floor = TRUE,
  unique = TRUE,
  sort = TRUE,
  units = dttr2::dtt_units(x[[dtt]]),
  .dts_fun = mean,
  ...
)
}
\arguments{
\item{x}{A Date or POSIXct vector.}

\item{dtt}{A string specifying the column with the Date or POSIXct values.}

\item{colname}{A character vector specifying the column(s) with the other values.}

\item{from}{A Date or POSIXct scalar of the start date.}

\item{to}{A Date or POSIXct scalar of the to date.}

\item{floor}{A flag indicating whether to floor the values.}

\item{unique}{A flag indicating whether to return distinct values.}

\item{sort}{A flag indicating whether the returned vector must be sorted.}

\item{units}{A string of the units.}

\item{.dts_fun}{A function which returns a scalar.}

\item{...}{Unused.}
}
\value{
The completed data time series.
}
\description{
Completes a data time series
}
\examples{
dts_complete(dts_data[c(1,3),])
}

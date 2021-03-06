% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colnames.R
\name{dts_colnames}
\alias{dts_colnames}
\title{DateTime Data Frame Column Names}
\usage{
dts_colnames(x, dtt = "DateTime")
}
\arguments{
\item{x}{A dts data frame}

\item{dtt}{A string specifying the column with the Date or POSIXct values.}
}
\value{
A character vector of the other column names.
}
\description{
DateTime Data Frame Column Names
}
\examples{
dts_colnames(dts_data)
}

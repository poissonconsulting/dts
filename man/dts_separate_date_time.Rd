% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/separate.R
\name{dts_separate_date_time}
\alias{dts_separate_date_time}
\alias{dts_separate_date}
\title{Separates the DateTime column of a data frame into its component parts.}
\usage{
dts_separate_date_time(
  x,
  dtt = "DateTime",
  year = TRUE,
  month = TRUE,
  day = TRUE,
  hour = TRUE,
  minute = TRUE,
  second = TRUE,
  dayte = FALSE,
  prefix = "",
  suffix = ""
)

dts_separate_date(
  x,
  dtt = "DateTime",
  year = TRUE,
  month = TRUE,
  day = TRUE,
  dayte = FALSE,
  prefix = "",
  suffix = ""
)
}
\arguments{
\item{x}{A data frame with a Date or POSIXct column.}

\item{dtt}{A string specifying the column with the Date or POSIXct values.}

\item{year}{A flag indicating whether to add a Year column.}

\item{month}{A flag indicating whether to add a Month column.}

\item{day}{A flag indicating whether to add a Day column.}

\item{hour}{A flag indicating whether to add a Hour column.}

\item{minute}{A flag indicating whether to add a Minute column.}

\item{second}{A flag indicating whether to add a Second column.}

\item{dayte}{A flag indicating whether to add a Dayte column.}

\item{prefix}{A string to add to the start of the column names.}

\item{suffix}{A string to add to the end of the column names.}
}
\value{
The updated data frame.
}
\description{
Separates the DateTime column of a data frame into its component parts.
}
\section{Functions}{
\itemize{
\item \code{dts_separate_date}: Separate Date
}}

\examples{
dts_separate_date_time(dts_data[c(1,3),])
}

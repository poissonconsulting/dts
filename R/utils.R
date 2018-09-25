#' Year, Month, Day, Hour, Minute or Second
#'
#' @inheritParams dts_interval
#' @return An integer vector
#' @export
#'
#' @examples
#' dts_year(dts_data$DateTime[1:10])
#' dts_month(dts_data$DateTime[1:10])
#' dts_day(dts_data$DateTime[1:10])
#' dts_hour(dts_data$DateTime[1:10])
#' dts_minute(dts_data$DateTime[1:10])
#' dts_second(dts_data$DateTime[1:10])
dts_year <- function(x) {
  check_vector(x, Sys.time())
  x <- as.POSIXlt(x)
  x$year + 1900L
}

#' @describeIn dts_year Month
#' @export
dts_month <- function(x) {
  check_vector(x, Sys.time())
  x <- as.POSIXlt(x)
  x$mon + 1L
}

#' @describeIn dts_year Day
#' @export
dts_day <- function(x) {
  check_vector(x, Sys.time())
  x <- as.POSIXlt(x)
  x$mday
}

#' @describeIn dts_year Hour
#' @export
dts_hour <- function(x) {
  check_vector(x, Sys.time())
  x <- as.POSIXlt(x)
  x$hour
}

#' @describeIn dts_year Minute
#' @export
dts_minute <- function(x) {
  check_vector(x, Sys.time())
  x <- as.POSIXlt(x)
  x$min
}

#' @describeIn dts_year Second
#' @export
dts_second <- function(x) {
  check_vector(x, Sys.time())
  x <- as.POSIXlt(x)
  x$sec
}

#' Interval
#' 
#' The possible interval values are "seconds", "minutes", "hours",
#' "days", "months" and "years"
#'
#' @param x A POSIXct object
#'
#' @return A string of the interval
#' @export
#'
#' @examples
#' dts_interval(dts_data$DateTime)
dts_interval <- function(x) {
  check_vector(x, Sys.time())
  if(!length(x)) return(character(0))
  if(any(dts_second(x) != 0)) return("seconds")
  if(any(dts_minute(x) != 0)) return("minutes")
  if(any(dts_hour(x) != 0)) return("hours")
  if(any(dts_day(x) != 1)) return("days")
  if(any(dts_month(x) != 1)) return("months")
  "years"  
}

#' Time Zone
#'
#' @param x A POSIXct object.
#' @return A string of the time zone.
#' @export
#'
#' @examples
#' dts_tz(dts_data$DateTime)
dts_tz <- function(x) {
  check_vector(x, Sys.time())
  attr(x, "tzone")
}

#' Complete
#' 
#' Tests whether a POSIXct object is complete.
#'
#' @inheritParams dts_tz
#' @return A flag indicating whether complete
#' @export
#'
#' @examples
#' dts_complete(dts_data$DateTime[c(1,2,3)])
#' dts_complete(dts_data$DateTime[c(1,3)])
#' dts_complete(dts_data$DateTime[c(1,3,2)]) 
dts_complete <- function(x) {
  check_vector(x, Sys.time())
  interval <- dts_interval(x)
  if(identical(interval, character(0))) return(TRUE)
  seq <- seq(min(x), max(x), by = interval)
  all(seq %in% x)
}


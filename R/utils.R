#' Arithmetic Mean Without Missing Values
#'
#' @param x An R object.
#'
#' @return The arithmetic mean of x after removing missing values.
#' @export
#'
#' @examples
#' mean_na_rm(c(1:2, NA))
mean_na_rm <- function(x) mean(x, na.rm = TRUE)

#' Normal Arrival-Departure Distribution Function
#' 
#' @param x A numeric vector of the times to calculate the distribution for.
#' @param mean A number of the mean arrival time.
#' @param sd A non-negative number of the standard deviation of the arrival timing.
#' @param residence A number of the residence time.
#' @return A numeric vector of the proportion present at x.
#' @export
#' @examples
#' x <- seq(-5,7,by = 0.1) 
#' plot(x, pnorm_arrival_departure(x), type = "l")
pnorm_arrival_departure <- function(x, mean = 0, sd = 1, residence = 6) {
  x <- check_double(x, 1, coerce = TRUE)
  check_number(mean)
  check_noneg_dbl(sd)
  check_number(residence)
  stats::pnorm(x, mean, sd) - stats::pnorm(x, mean + residence, sd)
}

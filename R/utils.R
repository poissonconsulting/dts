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
  x <- as.double(x)
  chk_double(x, 1)
  chk_dbl(mean)
  chk_dbl(sd)
  chk_gt(sd, 0)
  chk_dbl(residence)
  stats::pnorm(x, mean, sd) - stats::pnorm(x, mean + residence, sd)
}

#' Normalize
#' 
#' Normalizes the values in x so that they sum to 1.
#' Proportion indicates whether to set values to 0.
#'
#' @param x A numeric vector
#' @param proportion A number between 0 and 1
#'
#' @return A numeric vector of the normalized values.
#' @export
#'
#' @examples
#' normalize(c(0.1, 100, 10), proportion = 0.95)
normalize <- function(x, proportion = 1) {
  chk_vector(x)
  chk_gte(x)
  chk_double(proportion)
  chk_range(proportion, c(0, 1))
  
  if(!length(x)) return(x)
  
  x <- x / sum(x)
  
  y <- x
  values <- c(0, sort(unique(x)))
  for(i in seq_along(values)) {
    y[y == values[i]] <- 0
    if(sum(y) <= proportion) break
  }
  if(i > 1)
  x[x <= values[i-1]] <- 0
  x <- x / sum(x)
  x
}

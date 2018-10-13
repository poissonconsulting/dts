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

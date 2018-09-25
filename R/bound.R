#' Bound
#'
#' Values outside the bounds are set to missing values.
#' 
#' @inheritParams check_dts
#' @param bounds A sorted double vector of the lower and upper permitted bounds.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_bound(dts_data[1:5,], bounds = c(0,Inf))
dts_bound <- function(x, date_time = "DateTime", value = "Value",
                      bounds = c(-Inf, Inf)) {
  check_dts(x, date_time = date_time, value = value)
  check_vector(bounds, c(-Inf, Inf), length = 2L, unique = TRUE)
  check_sorted(bounds)
  
  x[[value]][x[[value]] < bounds[1]] <- NA_real_
  x[[value]][x[[value]] > bounds[2]] <- NA_real_
  x
}

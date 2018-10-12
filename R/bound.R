#' Bound
#'
#' Values outside the bounds are set to missing values.
#' 
#' @inheritParams check_dts
#' @param bounds A sorted double vector of the lower and upper permitted bounds.
#' @param adjust A flag indicating whether to adjust values outside the bounds to the permitted bounds
#'  or whether to set to NA (the default).
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_bound(dts_data[1:5,], bounds = c(0,Inf))
dts_bound <- function(x, date_time = "DateTime", value = "Value",
                      bounds = c(-Inf, Inf), adjust = FALSE) {
  check_string(value)
  check_dts(x, date_time = date_time, value = value)
  check_vector(bounds, length = 2L)
  check_flag(adjust)
  
  bounds <- sort(bounds)
  
  if(!adjust) {
    x[[value]][x[[value]] < bounds[1]] <- NA
    x[[value]][x[[value]] > bounds[2]] <- NA
  } else {
    x[[value]][x[[value]] < bounds[1]] <- bounds[1]
    x[[value]][x[[value]] > bounds[2]] <- bounds[2]
  }
  x
}

#' Interpolate
#'
#' Fill in missing values by linear interpolation.
#' 
#' @inheritParams check_dts
#' @param max_gap An integer of the maximum gap to interpolate by.
#' @param method A string of "linear" or "constant" indicating the method to use.
#' @param step A proportion specifing the compromise between 
#' left- and right- continuous step function.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_interpolate(dts_data[c(1,2,3),])
dts_interpolate <- function(x, date_time = "DateTime", value = "Value", 
                              max_gap = 10L, method = "linear", step = 0.5) {
  check_dts(x, date_time = date_time, value = value, sorted = TRUE, 
            complete = TRUE, key = date_time)
  max_gap <- check_count(max_gap, coerce = TRUE)
  check_scalar(method, c("linear", "constant", "constant"))
  check_prop(step)
  
  if (nrow(x) < 2L || max_gap == 0L) 
    return(x)
  
  gap <- size_gaps(is.na(x[[value]]))
  x[[value]] <- stats::approx(x[[value]], xout = 1:length(x[[value]]), 
                              method = method, f = step)$y
  is.na(x[[value]][gap > max_gap]) <- TRUE
  x
}

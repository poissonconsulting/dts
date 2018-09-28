#' Interpolate
#'
#' Fill in missing values by linear interpolation.
#' 
#' @inheritParams check_dts
#' @param max_span An integer of the maximum span to interpolate.
#' @param method A string of "linear" or "constant" indicating the method to use.
#' @param step A proportion specifing the compromise between 
#' left- and right- continuous step function.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_interpolate(dts_data[1:5,])
dts_interpolate <- function(x, date_time = "DateTime", value = "Value", 
                            max_span = .Machine$integer.max, 
                            method = "linear", step = 0.5) {
  check_dts(x, date_time = date_time, value = value, sorted = TRUE, 
            complete = TRUE, key = date_time)
  check_scalar(max_span, c(1L, .Machine$integer.max))
  check_scalar(method, c("linear", "constant", "constant"))
  check_prop(step)
  
  which <- which_replace(x[[value]], max_span = max_span, ends = FALSE)
  if(!length(which)) return(x)
  
  x[[value]][which] <- stats::approx(x[[value]], xout = which, 
                                     method = method, f = step)$y
  x
}

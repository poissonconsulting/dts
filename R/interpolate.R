#' Interpolate
#'
#' Fill in missing values by linear interpolation.
#' 
#' @inheritParams check_dts
#' @param max_span An integer of the maximum span to interpolate.
#' left- and right- continuous step function.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_interpolate(dts_data[1:5,])
dts_interpolate <- function(x, date_time = "DateTime", value = "Value", 
                            max_span = .Machine$integer.max) {
  check_dts(x, date_time = date_time, value = value, sorted = TRUE, 
            complete = TRUE, key = date_time)
  check_scalar(max_span, c(1L, .Machine$integer.max))

  which <- which_replace(x[[value]], max_span = max_span, ends = FALSE)
  if(!length(which)) return(x)
  
  x[[value]][which] <- stats::approx(x[[value]], xout = which)$y
  x
}

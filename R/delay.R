#' Delay a data time series
#'
#' @inheritParams dttr::dtt_complete
#' @inheritParams check_dts
#' @param delay A number (numeric scalar) of the delay.
#' @param units A string of the delay units.
#' @export
#'
#' @examples
#' dts_delay(dts_data[1:4,], delay = 2)
dts_delay <- function(x, dtt = "DateTime", colname = dts_colnames(x),
                      delay = 0, units = "days") {
  
  check_dts(x, dtt = dtt, colname = colname, nas = FALSE,
            sorted = TRUE, unique = TRUE, complete = TRUE)
  
  delay <- check_dbl(delay, coerce = TRUE)
  
  delay <- dtt_adjust_units(delay, units, dtt_units(x[[dtt]]))
  delay <- as.integer(delay)

  if(!nrow(x) || !delay || !length(colname)) return(x)
  
  x[colname] <- lapply(x[colname], delay, delay)

  x
}

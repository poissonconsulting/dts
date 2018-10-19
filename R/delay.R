#' Delay a data time series
#'
#' @inheritParams dttr::dtt_complete
#' @inheritParams check_dts
#' @param delay An int (integer scalar) of the delay.
#' @return The completed data time series.
#' @export
#'
#' @examples
#' dts_delay(dts_data[1:4,], delay = 2)
dts_delay <- function(x, dtt = "DateTime", colname = dts_colnames(x),
                      units = dttr::dtt_units(x[[dtt]]),
                      delay = 0L) {
  
  check_dts(x, dtt = dtt, colname = colname, nas = FALSE,
            sorted = TRUE, unique = TRUE, complete = TRUE)
  
  delay <- check_int(delay, coerce = TRUE)
  
  if(!nrow(x) || !delay || !length(colname)) return(x)
  
  x[colname] <- lapply(x[colname], delay, delay)

  x
}

#' Lag (or lead) columns in a Date Time Series data frame
#'
#' @inheritParams dttr::dtt_complete
#' @inheritParams check_dts
#' @param n A int (integer scalar) of the lag.
#' @param units A string of the lag time units.
#' @export
#'
#' @examples
#' dts_lag(dts_data[1:4,], n = 2)
dts_lag <- function(x, dtt = "DateTime", colname = dts_colnames(x),
                      n = 1L, units = dtt_units(x[[dtt]])) {
  
  check_dts(x, dtt = dtt, colname = colname, nas = FALSE,
            sorted = TRUE, unique = TRUE, complete = TRUE)
  
  n <- check_int(n, coerce = TRUE)

  if(!nrow(x) || !n || !length(colname)) return(x)
  
  n <- dtt_adjust_units(n, units, dtt_units(x[[dtt]]))
  n <- as.integer(n)

  x[colname] <- lapply(x[colname], delay, n)

  x
}

#' @rdname dts_lag
#' @export
dts_lead <- function(x, dtt = "DateTime", colname = dts_colnames(x),
                      n = 1L, units = dtt_units(x[[dtt]])) {
  dts_lag(x, dtt = dtt, colname = colname, n = n * -1, units = units)
}

#' @rdname dts_lag
#' @export
dts_delay <- function(x, dtt = "DateTime", colname = dts_colnames(x),
                      n = 1L, units = dtt_units(x[[dtt]])) {
  .Deprecated("dts_lag")
  dts_lag(x, dtt = dtt, colname = colname, n = n, units = units)
}

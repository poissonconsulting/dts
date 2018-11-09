#' Floors the Date or POSIXct column of a date time series data frame.
#'
#' @inheritParams dttr::dtt_complete
#' @inheritParams check_dts
#' @return The completed data time series.
#' @export
#'
#' @examples
#' dts_floor(dts_data[c(1,3),], units = "years")
dts_floor <- function(x, dtt = "DateTime", units = dttr::dtt_units(x[[dtt]])) {
  check_dts(x, dtt = dtt)
  
  x[[dtt]] <- dtt_floor(x[[dtt]], units = units)
  x
}

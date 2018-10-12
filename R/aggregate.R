#' Aggregate
#'
#' @inheritParams dts_check
#' @param .fun A function which returns a scalar.
#' @param ... Optional Additional arguments passed to .fun.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_aggregate(dts_data, units = "years", na.rm = TRUE)
dts_aggregate <- function(x, date_time = "DateTime", value = "Value", 
                          units = dttr::dtt_units(x[[date_time]]), .fun = mean, ...) {
  check_dts(x, date_time = date_time, value = value)
  x <- x[c(date_time, value)]
  x[[date_time]] <- dtt_floor(x[[date_time]], units = units)
  x <- split(x, x[[date_time]])
  x <- lapply(x, agg, .fun = .fun, ...)
  x <- do.call("rbind", x)
  rownames(x) <- NULL
  x
}

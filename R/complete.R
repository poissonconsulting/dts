#' Completes a data time series
#'
#' @inheritParams dtt_complete
#' @inheritParams check_dts
#' @return The completed data time series.
#' @export
#'
#' @examples
#' dts_complete(dts_data[c(1,3),])
dts_complete <- function(x, date_time = "DateTime", value = "Value",
                         floor = TRUE, 
                         sort = TRUE, unique = TRUE,
                         units = dttr::dtt_units(x[[date_time]])) {
  check_dts(x, date_time = date_time, value = value)
  
  seq <- dtt_complete(x[[date_time]], units = units)
  x <- data.frame(seq, c(x[[value]], rep(NA, length(seq) - nrow(x))))
  names(x) <- c(date_time, value)
  x <- x[order(x[[date_time]]),]
  if(requireNamespace("tibble", quietly = TRUE)) x <- tibble::as_tibble(x)
  rownames(x) <- NULL
  x
}

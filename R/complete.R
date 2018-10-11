#' Test if a data time series is complete
#'
#' @inheritParams check_dts
#'
#' @return A flag indicating whether complete.
#' @export
#'
#' @examples
#' dts_completed(dts_data[c(1,3),])
dts_completed <- function(x, date_time = "DateTime", value = "Value", 
                          units = dttr::dtt_units(x[[date_time]])) {
  
  check_dts(x, date_time = date_time, value = value)
  return(dtt_completed(x[[date_time]], units = units))
}


#' Completes a data time series
#'
#' @inheritParams check_dts
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_complete(dts_data[c(1,3),])
dts_complete <- function(x, date_time = "DateTime", value = "Value", 
                          units = dttr::dtt_units(x[[date_time]])) {
  
  check_dts(x, date_time = date_time, value = value)
  if(dts_completed(x, date_time = date_time, value = value, units = units))
    return(x[c(date_time, value)])
  
  seq <- dtt_complete(x[[date_time]])
  x <- data.frame(seq, c(x[[value]], rep(NA_real_, length(seq) - nrow(x))))
  
  names(x) <- c(date_time, value)
  x <- x[order(x[[date_time]]),]
  if(requireNamespace("tibble", quietly = TRUE)) x <- tibble::as_tibble(x)
  rownames(x) <- NULL
  x
}

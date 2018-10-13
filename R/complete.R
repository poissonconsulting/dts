#' Completes a data time series
#'
#' @inheritParams dttr::dtt_complete
#' @inheritParams check_dts
#' @inheritParams dts_aggregate
#' @return The completed data time series.
#' @export
#'
#' @examples
#' dts_complete(dts_data[c(1,3),])
dts_complete <- function(x, date_time = "DateTime", colname = dts_colnames(x),
                         floor = TRUE, 
                         unique = TRUE, sort = TRUE, 
                         units = dttr::dtt_units(x[[date_time]]),
                         .fun = mean, ...) {
  check_dts(x, date_time = date_time, colname = colname)
  
  x <- x[c(date_time, colname)]
  
  if(floor) x[[date_time]] <- dtt_floor(x[[date_time]], units = units)
  if(unique) x <- dts_aggregate(x, date_time = date_time, colname = colname,
                                .fun = .fun, ...)

  seq <- dtt_complete(x[[date_time]], floor = FALSE, unique = FALSE, 
                      sort = FALSE, units = units)
  
  data <- data.frame(seq)
  names(data) <- date_time
  
  x <- merge(x, data, all = TRUE, sort = sort, by = date_time)
  if(requireNamespace("tibble", quietly = TRUE)) x <- tibble::as_tibble(x)
  rownames(x) <- NULL
  x
}

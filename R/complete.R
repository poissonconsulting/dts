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
dts_complete <- function(x, dtt = "DateTime", colname = dts_colnames(x),
                         floor = TRUE, 
                         unique = TRUE, sort = TRUE, 
                         units = dttr::dtt_units(x[[dtt]]),
                         .fun = mean, ...) {
  check_dts(x, dtt = dtt, colname = colname)
  
  x <- x[c(dtt, colname)]
  
  if(floor) x[[dtt]] <- dtt_floor(x[[dtt]], units = units)
  if(unique) x <- dts_aggregate(x, dtt = dtt, colname = colname,
                                .fun = .fun, ...)

  seq <- dtt_complete(x[[dtt]], floor = FALSE, unique = FALSE, 
                      sort = FALSE, units = units)
  
  data <- data.frame(seq)
  names(data) <- dtt
  
  x <- merge(x, data, all = TRUE, sort = sort, by = dtt)
  if(requireNamespace("tibble", quietly = TRUE)) x <- tibble::as_tibble(x)
  rownames(x) <- NULL
  x
}

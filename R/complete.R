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
                         from = min(x[[dtt]]), to = max(x[[dtt]]),
                         floor = TRUE, 
                         unique = TRUE, sort = TRUE, 
                         units = dttr2::dtt_units(x[[dtt]]),
                         .dts_fun = mean, ...) {
  check_dts(x, dtt = dtt, colname = colname, nas = FALSE)
  
  x <- x[c(dtt, colname)]
  
  if(floor) x <- dts_floor(x, dtt, units = units)
  if(unique) x <- dts_aggregate(x, dtt = dtt, colname = colname, units = units,
                                .dts_fun = .dts_fun, ...)

  seq <- dtt_complete(x[[dtt]], from = from, to = to, floor = FALSE, unique = FALSE, 
                      sort = FALSE, units = units)
  
  data <- data.frame(seq)
  names(data) <- dtt
  
  x <- merge(x, data, all = TRUE, sort = sort, by = dtt)
  if(requireNamespace("tibble", quietly = TRUE)) x <- tibble::as_tibble(x)
  rownames(x) <- NULL
  x
}

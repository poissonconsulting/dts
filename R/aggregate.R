#' Aggregate
#'
#' @inheritParams check_dts
#' @param .dts_fun A function which returns a scalar.
#' @param ... Optional Additional arguments passed to .dts_fun.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_aggregate(dts_data, units = "years", na.rm = TRUE)
dts_aggregate <- function(x, dtt = "DateTime", colname = dts_colnames(x), 
                          units = dttr::dtt_units(x[[dtt]]), .dts_fun = mean, ...) {
  check_dts(x, dtt = dtt, colname = colname)
  x <- x[c(dtt, colname)]
  rownames(x) <- NULL
  if(!nrow(x)) return(x)
  
  x <- dts_floor(x, dtt, units = units)
  if(length(colname)) {
    x <- split(x, x[[dtt]])
    x <- lapply(x, agg, .dts_fun = .dts_fun, ...)
    x <- do.call("rbind", x)
  }
  rownames(x) <- NULL
  x
}

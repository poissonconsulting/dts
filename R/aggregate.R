#' Aggregate
#'
#' @inheritParams check_dts
#' @param .fun A function which returns a scalar.
#' @param ... Optional Additional arguments passed to .fun.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_aggregate(dts_data, units = "years", na.rm = TRUE)
dts_aggregate <- function(x, dtt = "DateTime", colname = dts_colnames(x), 
                          units = dttr::dtt_units(x[[dtt]]), .fun = mean, ...) {
  check_dts(x, dtt = dtt, colname = colname)
  x <- x[c(dtt, colname)]
  rownames(x) <- NULL
  if(!nrow(x)) return(x)
  
  x[[dtt]] <- dtt_floor(x[[dtt]], units = units)
  if(length(colname)) {
    x <- split(x, x[[dtt]])
    x <- lapply(x, agg, .fun = .fun, ...)
    x <- do.call("rbind", x)
  }
  rownames(x) <- NULL
  x
}

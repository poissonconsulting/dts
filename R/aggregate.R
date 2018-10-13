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
dts_aggregate <- function(x, date_time = "DateTime", colname = dts_colnames(x), 
                          units = dttr::dtt_units(x[[date_time]]), .fun = mean, ...) {
  check_dts(x, date_time = date_time, colname = colname)
  x <- x[c(date_time, colname)]
  rownames(x) <- NULL
  if(!nrow(x)) return(x)
  
  x[[date_time]] <- dtt_floor(x[[date_time]], units = units)
  if(length(colname)) {
    x <- split(x, x[[date_time]])
    x <- lapply(x, agg, .fun = .fun, ...)
    x <- do.call("rbind", x)
  }
  rownames(x) <- NULL
  x
}

#' Disaggregate
#'
#' @inheritParams check_dts
#'
#' @return The disaggregated data frame
#' @export
#'
#' @examples
#' dts_disaggregate(dts_aggregate(dts_data[1,], units = "years"))
dts_disaggregate <- function(x, date_time = "DateTime", colname = dts_colnames(x)) {
  check_dts(x, date_time = date_time, colname = colname, 
            sorted = TRUE, unique = TRUE, complete = TRUE)
  
  x <- x[c(date_time, colname)]
  check_missing_colnames(x, dot(date_time))
  
  data <- data.frame(dtt_disaggregate(x[[date_time]]))
  colnames(data) <- dot(date_time)
  data[[date_time]] <- dtt_floor(data[[dot(date_time)]], units = dtt_units(x[[date_time]]))
  
  x <- merge(x, data, by = date_time, all = TRUE)
  x[[date_time]] <- x[[dot(date_time)]]
  x[[dot(date_time)]] <- NULL
  x <- x[order(x[[date_time]]),,drop = FALSE]
  x
}

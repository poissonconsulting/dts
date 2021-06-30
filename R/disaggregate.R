#' Disaggregate
#'
#' @inheritParams check_dts
#'
#' @return The disaggregated data frame
#' @export
#'
#' @examples
#' dts_disaggregate(dts_aggregate(dts_data[1,], units = "years"))
dts_disaggregate <- function(x, dtt = "DateTime", colname = dts_colnames(x)) {
  check_dts(x, dtt = dtt, colname = colname, 
            sorted = TRUE, unique = TRUE, complete = TRUE)
  
  x <- x[c(dtt, colname)]
  chk_not_subset(names(x), dot(dtt))

  data <- data.frame(dtt_disaggregate(x[[dtt]]))
  colnames(data) <- dot(dtt)
  data[[dtt]] <- dtt_floor(data[[dot(dtt)]], units = dtt_units(x[[dtt]]))
  
  x <- merge(x, data, by = dtt, all = TRUE)
  x[[dtt]] <- x[[dot(dtt)]]
  x[[dot(dtt)]] <- NULL
  x <- x[order(x[[dtt]]),,drop = FALSE]
  x
}

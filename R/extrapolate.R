#' Extrapolation
#'
#' Fill in missing values by padding with the first or last non-missing value.
#' 
#' @inheritParams check_dts
#' @param max_span An integer of the maximum span to extrapolate.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_extrapolate(dts_data[2:6,])
dts_extrapolate <- function(x, dtt = "DateTime", colname = dts_colnames(x),
                            max_span = .Machine$integer.max) {
  check_dts(x, dtt = dtt, colname = colname, sorted = TRUE,
            unique = TRUE)
  check_scalar(max_span, c(1L, .Machine$integer.max))
  
  if(!length(colname)) return(x)
  x[colname] <- lapply(x[colname], extrapolate, max_span)
  x
}

#' F
#'
#' Fill in missing values by linear interpolation.
#' 
#' @inheritParams check_dts
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_extrapolate(dts_data[2:6,])
dts_extrapolate <- function(x, dtt = "DateTime", colname = dts_colnames(x)) {
  check_dts(x, dtt = dtt, colname = colname, sorted = TRUE,
            unique = TRUE)

  if(!length(colname)) return(x)
  x[colname] <- lapply(x[colname], extrapolate)
  x
}

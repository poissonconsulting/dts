#' Interpolate
#'
#' Fill in missing values by linear interpolation.
#' 
#' @inheritParams check_dts
#' @param max_span An integer of the maximum span to interpolate.
#' left- and right- continuous step function.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_interpolate(dts_data[1:5,])
dts_interpolate <- function(x, dtt = "DateTime", colname = dts_colnames(x), 
                            max_span = .Machine$integer.max) {
  check_dts(x, dtt = dtt, colname = colname, sorted = TRUE,
            unique = TRUE, complete = TRUE)
  chk_scalar(max_span)
  chk_gte(max_span, 1L)

  if(!length(colname)) return(x)
  x[colname] <- lapply(x[colname], interpolate, max_span)
  x
}

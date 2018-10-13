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
  check_scalar(max_span, c(1L, .Machine$integer.max))

  if(!length(colname)) return(x)
  x[2:ncol(x)] <- lapply(x[2:ncol(x)], interpolate, max_span)
  x
}

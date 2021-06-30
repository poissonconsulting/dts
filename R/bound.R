#' Bound
#'
#' Values outside the bounds are set to missing values.
#' 
#' @inheritParams check_dts
#' @param bounds A sorted double vector of the lower and upper permitted bounds.
#' @param adjust A flag indicating whether to adjust values outside the bounds to the permitted bounds
#'  or whether to set to NA (the default).
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_bound(dts_data[1:5,], bounds = c(0,Inf))
dts_bound <- function(x, dtt = "DateTime", colname = "Value",
                      bounds = c(-Inf, Inf), adjust = FALSE) {
  chk_string(colname)
  check_dts(x, dtt = dtt, colname = colname)
  chk_vector(bounds)
  check_dim(bounds, values = 2L)
  chk_flag(adjust)
  
  bounds <- sort(bounds)
  
  if(!adjust) {
    x[[colname]][x[[colname]] < bounds[1]] <- NA
    x[[colname]][x[[colname]] > bounds[2]] <- NA
  } else {
    x[[colname]][x[[colname]] < bounds[1]] <- bounds[1]
    x[[colname]][x[[colname]] > bounds[2]] <- bounds[2]
  }
  x
}

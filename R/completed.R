#' Test if the DateTime in a DateTime-Series is complete.
#'
#' @inheritParams check_dts
#'
#' @return A flag indicating whether complete.
#' @seealso \code{\link[dttr]{dtt_completed}}
#' @export
#'
#' @examples
#' dts_completed(dts_data[c(1,3),])
dts_completed <- function(x, date_time = "DateTime", 
                          floored = TRUE, unique = TRUE, sorted = TRUE, 
                          units = dttr::dtt_units(x[[date_time]])) {
  check_dts(x, date_time = date_time)

  dtt_completed(x[[date_time]], floored = floored, sorted = sorted, 
                unique = unique, units = units)
}

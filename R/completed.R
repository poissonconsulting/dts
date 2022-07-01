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
dts_completed <- function(x, dtt = "DateTime", 
                          floored = TRUE, unique = TRUE, sorted = TRUE, 
                          units = dttr2::dtt_units(x[[dtt]])) {
  check_dts(x, dtt = dtt)
  
  if(floored && !dtt_floored(x[[dtt]], units = units)) return(FALSE)
  if(!floored & dtt_floored(x[[dtt]], units = units)) return(FALSE)
  x[[dtt]] <- dtt_floor(x[[dtt]], units)
  dtt_completed(x[[dtt]], sorted = sorted, 
                  unique = unique, units = units)
}

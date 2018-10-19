#' Accumulated
#'
#' Calculates DateTime when reach accumulated total.
#' 
#' @inheritParams check_dts
#' @param accum A uniquely named vector of the accumulation thresholds.
#' @param units A string of the accumulation units.
#' If the accumulation units differ from the dtt units the accumulation units 
#' are automatically converted.
#'
#' @return The modified DateTime series data frame.
#' @export
#'
#' @examples
#' dts_accumulated(dts_data[1:5,], colname = "Value2", 
#'   accum = c(Hatch = 40), units = "hours")
dts_accumulated <- function(x, dtt = "DateTime", colname = "Value", 
                            accum = c(Hatch = 320),
                            units = "days") {
  check_string(colname)
  check_dts(x, dtt = dtt, colname = colname, sorted = TRUE,
            unique = TRUE, complete = TRUE)
  check_vector(accum, c(0, chk_max_dbl()))
  check_named(accum, unique = TRUE)
  check_missing_names(accum, c(dtt, colname))

  if(!length(accum)) return(x)
  
  accum <- dtt_adjust_units(accum, units, dtt_units(x[[dtt]]))

  wch <- lapply(accum, calc_atus, cumsum(x[[colname]]))
  x[names(wch)] <- set_na(x[[dtt]][1])
  for(col in names(wch)) {
    x[[col]][wch[[col]] != 0L] <- x[[dtt]][wch[[col]]]
  }
  x
}

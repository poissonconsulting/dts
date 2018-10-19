#' Accumulated
#'
#' Calculates DateTime when reach accumulated total.
#' 
#' @inheritParams check_dts
#' @param accum A uniquely named list do durations.
#'
#' @return The modified DateTime series data frame.
#' @export
#'
#' @examples
#' dts_accumulated(dts_data[1:5,], colname = "Value2", 
#'   accum = list(Hatch = dttr::dtt_duration(40, "hours")))
dts_accumulated <- function(x, dtt = "DateTime", colname = "Value", 
                            accum = list(Hatch = dttr::dtt_duration(320, "days"))) {
  check_string(colname)
  check_dts(x, dtt = dtt, colname = colname, sorted = TRUE,
            unique = TRUE, complete = TRUE)
  check_named(accum, unique = TRUE)
  lapply(accum, check_duration, x_name = "elements of accum")
  check_missing_names(accum, c(dtt, colname))

  if(!length(accum)) return(x)
  
  units <- dtt_units(x[[dtt]])
  
  accum <- lapply(accum, function(d) { 
    as_numeric(d) / dtt_units_per_unit(unit = units)})

  wch <- lapply(accum, calc_atus, cumsum(x[[colname]]))
  x[names(wch)] <- set_na(x[[dtt]][1])
  for(col in names(wch)) {
    x[[col]][wch[[col]] != 0L] <- x[[dtt]][wch[[col]]]
  }
  x
}

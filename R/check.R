#' Check DateTime-Series Data Frame
#' 
#' A dts data frame is a data frame with a column of non-missing Date or POSIXct values and
#' at least one column of numeric values.
#'
#' @param x A dts data frame
#' @param date_time A string specifying the column with the Date or POSIXct values.
#' @param value A string specifying the column with the double values.
#' @inheritParams checkr::check_data
#' @param sorted A flag indicating whether the DateTime values must be sorted (in ascending order).
#' @param complete A flag indicating whether the DateTime values must be complete.
#' @param units A string of the time units.
#' @return An invisible copy of x (if it doesn't throw an error).
#' @export
#'
#' @examples
#' check_dts(dts_data)
check_dts <- function(x, date_time = "DateTime", value = "Value", 
                      nrow = NA, sorted = FALSE, complete = FALSE,
                      units = dttr::dtt_units(x[[date_time]]),
                      key = character(0),
                      x_name = substitute(x)) {
  x_name <- deparse(x_name)

  check_string(date_time)
  check_string(value)
  check_flag(sorted)
  check_flag(complete)
  check_string(x_name)
  
  if(identical(date_time, value))
    err("arguments 'date_time' and 'value' must specify different columns")
  
  if(anyNA(x[[date_time]]))
    err("column '", date_time, "' of ", x_name, "must not include missing values")
  
  checkor(
    check_vector(
      x[[date_time]], Sys.Date(), x_name = 
        paste0("column '", date_time, "' of ", x_name, " must be class Date")),
    check_vector(
      x[[date_time]], Sys.time(), x_name = 
        paste0("column '", date_time, "' of ", x_name, " must be class POSIXct"))
  )
  
  check_data(x, nrow = nrow, key = key, x_name = x_name)
  
  if(sorted)
    check_sorted(x[[date_time]], x_name = paste0("column '", date_time, "' of ", x_name))

  if(complete && !dtt_completed(x[[date_time]], units = units))
    err("column '", date_time, "' of ", x_name, " must be complete")
  
  invisible(x)
}

#' Check DateTime-Series Data Frame
#' 
#' A dts data frame is a data frame with a column of non-missing POSIXct values
#' and a column of numeric values.
#'
#' @inheritParams checkr::check_data
#' @param date_time A string specifying the column with the POSIXct values.
#' @param value A string specifying the column with the numeric values.
#' @param sorted A flag indicating whether the DateTime values must be sorted (in ascending order).
#' @param complete A flag indicating whether the DateTime values must be complete.
#' @return An invisible copy of x (if it doesn't throw an error).
#' @export
#'
#' @examples
#' check_dts(dts_data)
check_dts <- function(x, date_time = "DateTime", value = "Value", 
                      nrow = NA, key = "DateTime", 
                      sorted = FALSE, complete = TRUE, 
                      x_name = substitute(x), error = TRUE) {
  x_name <- deparse(x_name)
  
  check_string(date_time)
  check_string(value)
  check_flag(complete)
  check_flag(sorted)
  check_string(x_name)
  
  if(identical(date_time, value))
    err("'date_time' and 'value' must specify different columns")
  
  values <- list(Sys.time(), c(1, NA))
  names(values) <- c(date_time, value)
  
  check_data(x, values = values, nrow = nrow, key = key, x_name = x_name,
             error = error)
  
  if(sorted) check_sorted(
    x[[date_time]],  x_name = paste0("column '", date_time, "' of ", x_name), 
    error = error
  )
  
  if(complete && !dts_complete(x[[date_time]]))
    err("column '", date_time, "' of ", x_name, "must be complete")
  invisible(x)
}

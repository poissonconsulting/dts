#' Check DateTime-Series Data Frame
#' 
#' A dts data frame is a data frame with a column of Date or POSIXct values.
#'
#' @param x A dts data frame
#' @param date_time A string specifying the column with the Date or POSIXct values.
#' @param value A character vector specifying the column(s) with the other values.
#' @inheritParams checkr::check_data
#' @inheritParams dttr::check_dtt
#' @return An invisible copy of x (if it doesn't throw an error).
#' @export
#'
#' @examples
#' check_dts(dts_data)
check_dts <- function(x, date_time = "DateTime", value = character(0), 
                      nrow = NA, floored = TRUE, sorted = FALSE, unique = FALSE, 
                      complete = FALSE,
                      units = dttr::dtt_units(x[[date_time]]),
                      tz = dttr::dtt_tz(x[[date_time]]),
                      exclusive = FALSE, order = FALSE,
                      key = character(0),
                      x_name = substitute(x), error = TRUE) {
  x_name <- chk_deparse(x_name)
  
  check_string(date_time)
  check_vector(value, "")

  check_data(x, c(date_time, value), nrow = nrow, 
             exclusive = exclusive, order = order, key = key, x_name = x_name, 
             error = TRUE)
  
  check_dtt(x[[date_time]], floored = floored, sorted = sorted, 
            unique = unique, complete = complete, 
            units = units, tz = tz, x_name = 
              paste0("column '", date_time, "' of ", x_name))
  invisible(x)
}

#' Check DateTime-Series Data Frame
#' 
#' A dts data frame is a data frame with a column of Date or POSIXct values.
#'
#' @param x A dts data frame
#' @param dtt A string specifying the column with the Date or POSIXct values.
#' @param colname A character vector specifying the column(s) with the other values.
#' @inheritParams checkr::check_data
#' @inheritParams dttr::check_dtt
#' @return An invisible copy of x (if it doesn't throw an error).
#' @export
#'
#' @examples
#' check_dts(dts_data)
check_dts <- function(x, dtt = "DateTime", colname = character(0), 
                      nrow = NA, nas = TRUE, floored = TRUE, 
                      sorted = FALSE, unique = FALSE, 
                      complete = FALSE,
                      units = dttr::dtt_units(x[[dtt]]),
                      tz = dttr::dtt_tz(x[[dtt]]),
                      exclusive = FALSE, order = FALSE,
                      x_name = NULL, error = TRUE) {
  if (is.null(x_name)) 
    x_name <- deparse_backtick_chk((substitute(x))) 
  chk_string(x_name)   
  chk_string(dtt)
  chk_vector(colname)
  check_values(colname, "")

  check_data(x, values = c(dtt, colname), nrow = nrow, exclusive = exclusive, 
             order = order, x_name = x_name)

  check_dtt(x[[dtt]], nas = nas, floored = floored, sorted = sorted, 
            unique = unique, complete = complete, 
            units = units, tz = tz, x_name = 
              paste0("column '", dtt, "' of ", x_name))
  invisible(x)
}

#' Distribution
#'
#' Calculates distribution by DateTime.
#' 
#' The date time values in x are converted into a numeric vector and 
#' passed as the first argument to .dts_fun.
#' 
#' @inheritParams check_dts
#' @param units A string of the time units for the variation (3rd and more) 
#' arguments of .dts_fun.
#' @param normalize A flag indicating whether to normalize the values (ensure they sum to 1) or
#' the proportion of the summed values to retain before renormalizing.
#' @param .dts_fun A string of the name of a function which takes a numeric vector as its first argument 
#' (the values to calculate the distribution for), a numeric scalar as it second argument
#' (the timing of the event) and one or more numeric scalars (representing the
#' variation in the timing of the event).
#' @param .timing A Date or POSIXct vector each element of which 
#' will be converted into a numeric value to be passed as the second argument to
#' .dts_fun.
#' @param ... Additional arguments passed as named arguments to .dts_fun 
#' which should all be numeric vectors
#' of the same length as .dtt or numeric scalars (which are reused). 
#' @return The modified DateTime series data frame with a column of the distribution.
#' @seealso \code{\link{normalize}}
#' @export
#'
#' @examples
#' dts_distribution(dts_data[1:10,], .timing = dts_data$DateTime[2], sd = 2, units = "hours")
dts_distribution <- function(x, dtt = "DateTime", colname = "Distribution", 
                        units = "days", normalize = TRUE, .dts_fun = "dnorm", .timing, ...) {
  chk_string(colname)
  check_dts(x, dtt = dtt, sorted = TRUE, unique = TRUE, complete = TRUE)
  if(colname == dtt) err("colname must not be '", dtt, "'")
  if (is.null(x)) 
    x <- deparse_backtick_chk(x) 
  chk_string(units)
  chk_scalar(units)
  chkor(chk_flag(normalize), chk_dbl(normalize), chk_range(normalize))
  chk_string(.dts_fun)
  is_date_time(.timing)
  chk_unique(.timing)
  chk_not_any_na(.timing)

  args <- list(...)

  lapply(args, check_dim, values = c(1L, 1L, length(.timing)))
  lapply(args, check_values, values = 1)
  
  
  x[[colname]] <- 0
  if(!length(.timing)) return(x)
  
  if(is.Date(x[[dtt]])) {
    .timing <- dtt_date(.timing)
    args <- lapply(args, dtt_adjust_units, from = units, to = "days")
  } else {
    .timing <- dtt_date_time(.timing, tz = dtt_tz(x[[dtt]]))
    args <- lapply(args, dtt_adjust_units, from = units, to = "seconds")
  }

  args <- lapply(args, function(x) rep(x, length(.timing) - length(x) + 1L))
  .dtt <- as.numeric(x[[dtt]])
  .timing <- as.numeric(.timing)
  
  for(i in seq_along(.timing)) {
    arg <- lapply(args, function(x, i) x[i], i = i)
    arg <- c(list(.dtt), .timing[i], arg)
    x[[colname]] <- x[[colname]] + do.call(.dts_fun, arg)
  }
  if(isTRUE(normalize)) {
    x[[colname]] <- normalize(x[[colname]])
  } else if (!isFALSE(normalize))
    x[[colname]] <- normalize(x[[colname]], proportion = normalize)
  x
}

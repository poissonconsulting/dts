#' Distribution
#'
#' Calculates distribution by DateTime.
#' 
#' The date time values in x are converted into a numeric vector and 
#' passed as the first argument to .fun.
#' 
#' @inheritParams check_dts
#' @param units A string of the time units for the variation (3rd and more) 
#' arguments of .fun.
#' @param normalize A flag indicating whether to normalize the values (ensure they sum to 1).
#' @param .fun A string of the name of a function which takes a numeric vector as its first argument 
#' (the values to calculate the distribution for), a numeric scalar as it second argument
#' (the timing of the event) and one or more numeric scalars (representing the
#' variation in the timing of the event).
#' @param .timing A Date or POSIXct vector each element of which 
#' will be converted into a numeric value to be passed as the second argument to
#' .fun.
#' @param ... Additional arguments passed as named arguments to .fun 
#' which should all be numeric vectors
#' of the same length as .dtt or numeric scalars (which are reused). 
#' @return The modified DateTime series data frame with a column of the distribution
#' @export
#'
#' @examples
#' dts_distribution(dts_data[1:10,], .timing = dts_data$DateTime[2], sd = 2, units = "hours")
dts_distribution <- function(x, dtt = "DateTime", colname = "Distribution", 
                        units = "days", normalize = TRUE, .fun = "dnorm", .timing, ...) {
  check_string(colname)
  check_dts(x, dtt = dtt, sorted = TRUE, unique = TRUE, complete = TRUE)
  if(colname == dtt) err("colname must not be '", dtt, "'")
  check_time_units(units)
  check_flag(normalize)
  check_string(.fun)
  check_dtt(.timing, nas = FALSE, unique = TRUE)
  
  args <- eval(substitute(alist(...)))
  
  lapply(args, check_vector, values = 1, length = c(1L, 1L, length(.timing)))
  
  x[[colname]] <- 0
  if(!length(.timing)) return(x)
  
  if(dtt_is_date(x[[dtt]])) {
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
    x[[colname]] <- x[[colname]] + do.call(.fun, arg)
  }
  if(normalize)
    x[[colname]] <- x[[colname]] / sum(x[[colname]])
  x
}

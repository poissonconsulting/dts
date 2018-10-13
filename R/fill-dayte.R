#' Fill Date or POSIXct of the Year
#'
#' Fills in missing values based on Date or DateTime of year.
#' 
#' @inheritParams check_dts
#' @inheritParams dts_aggregate
#' @inheritParams dts_regress
#' @param min_gap An integer of the minimum gap to leave between existing values.
#' @param feb29_to_28 A flag indicating whether to treat February 29th as if it is February 28th.
#'
#' @return A data frame
#' @export
dts_fill_dayte <- function(x, date_time = "DateTime", colname = dts_colnames(x), 
                                min_gap = 0L, min_n = 1L, feb29_to_28 = TRUE, 
                                .fun = mean, ...) {
  check_dts(x, date_time = date_time, colname = colname, unique = TRUE)
  check_missing_colnames(x, dot(c(date_time, colname)))
  check_count(min_gap)
  check_count(min_n)
  check_flag(feb29_to_28)

  if(!nrow(x) || !length(colname)) return(x)

  x[[dot(date_time)]] <- x[[date_time]]
  dtt_years(x[[dot(date_time)]]) <- 1972L
  
  data <- x[c(date_time, colname)]
  colnames(data) <- dot(colnames(data))
  dtt_years(data[[dot(date_time)]]) <- 1972L
  
  if(feb29_to_28) {
    data[[dot(date_time)]] <- dtt_feb29_to_28(data[[dot(date_time)]])
    x[[dot(date_time)]] <- dtt_feb29_to_28(x[[dot(date_time)]])
  }
  
  n <- dts_aggregate(data, date_time = dot(date_time), 
                     colname = dot(colname), .fun = function(x) sum(!is.na(x)))
  data <- dts_aggregate(data, date_time = dot(date_time), 
                        colname = dot(colname), .fun = .fun, ...)
  
  stopifnot(identical(n[[dot(date_time)]], data[[dot(date_time)]]))
  
  for(col in colname)
    is.na(data[[dot(col)]][n[[dot(col)]] < min_n]) <- TRUE
  rm(n)

  x <- merge(x, data, by = dot(date_time), all = TRUE, sort = FALSE)
  rm(data)
  x <- x[order(x[[date_time]]),]
  
  for(col in colname) {
    wch <- which_replace(x[[col]], min_gap = min_gap)
    if(length(wch)) x[[col]][wch] <- x[[dot(col)]][wch]
  }
  x[dot(c(date_time, colname))] <- NULL
  if(requireNamespace("tibble", quietly = TRUE)) x <- tibble::as_tibble(x)
  rownames(x) <- NULL
  x
}

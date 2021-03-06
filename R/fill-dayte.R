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
dts_fill_dayte <- function(x, dtt = "DateTime", colname = dts_colnames(x), 
                                min_gap = 0L, min_n = 1L, feb29_to_28 = TRUE, 
                                .dts_fun = mean_na_rm, ...) {
  check_dts(x, dtt = dtt, colname = colname, unique = TRUE)
  chk_not_subset(names(x), dot(c(dtt, colname)))
  chk_whole_number(min_gap)
  chk_gte(min_gap)
  chk_whole_number(min_n)
  chk_gte(min_n)
  
  chk_flag(feb29_to_28)

  if(!nrow(x) || !length(colname)) return(x)

  x[[dot(dtt)]] <- x[[dtt]]
  dtt_year(x[[dot(dtt)]]) <- 1972L
  
  data <- x[c(dtt, colname)]
  colnames(data) <- dot(colnames(data))
  dtt_year(data[[dot(dtt)]]) <- 1972L
  
  if(feb29_to_28) {
    data[[dot(dtt)]] <- dtt_feb29_to_28(data[[dot(dtt)]])
    x[[dot(dtt)]] <- dtt_feb29_to_28(x[[dot(dtt)]])
  }
  
  n <- dts_aggregate(data, dtt = dot(dtt), 
                     colname = dot(colname), .dts_fun = function(x) sum(!is.na(x)))
  data <- dts_aggregate(data, dtt = dot(dtt), 
                        colname = dot(colname), .dts_fun = .dts_fun, ...)
  
  stopifnot(identical(n[[dot(dtt)]], data[[dot(dtt)]]))
  
  for(col in colname)
    is.na(data[[dot(col)]][n[[dot(col)]] < min_n]) <- TRUE
  rm(n)

  x <- merge(x, data, by = dot(dtt), all = TRUE, sort = FALSE)
  rm(data)
  x <- x[order(x[[dtt]]),]
  
  for(col in colname) {
    wch <- which_replace(x[[col]], min_gap = min_gap)
    if(length(wch)) x[[col]][wch] <- x[[dot(col)]][wch]
  }
  x[dot(c(dtt, colname))] <- NULL
  if(requireNamespace("tibble", quietly = TRUE)) x <- tibble::as_tibble(x)
  rownames(x) <- NULL
  x
}

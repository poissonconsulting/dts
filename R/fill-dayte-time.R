#' Fill Date or DateTime of the Year
#'
#' Fills in missing values based on Date or DateTime of year.
#' 
#' @inheritParams check_dts
#' @inheritParams dts_aggregate
#' @inheritParams dts_regress
#' @param min_gap An integer of the minimum gap to leave between existing values.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_fill_dayte_time(dts_data, min_gap = 0L)[1:5,]
dts_fill_dayte_time <- function(x, date_time = "DateTime", value = "Value", 
                         min_gap = 10L, min_n = 2L, 
                         units = dttr::dtt_units(x[[date_time]]), .fun = mean, ...) {
  
  check_dts(x, date_time = date_time, value = value, sorted = TRUE, 
            complete = TRUE, key = date_time)
  check_count(min_gap)
  check_scalar(min_n, c(1L, .Machine$integer.max))
  check_missing_colnames(x, c("..DayteTime", "..Value"))
  
  which <- which_replace(x[[value]], min_gap = min_gap)
  if(!length(which)) return(x)
  
  data <- x[c(date_time, value)]
  names(data) <- c("DateTime", "Value")
  data <- data[!is.na(data$Value),]
  if(!nrow(data)) return(x)
  
  dtt_years(data$DateTime) <- 1972L
  n <- dts_aggregate(data, units = units, .fun = length)
  data <- dts_aggregate(data, units = units, .fun = .fun, ...)
  data <- data[n$Value >= min_n,]
  if(!nrow(data)) return(x)
  
  x$..DayteTime <- x[[date_time]]
  dtt_years(x$..DayteTime) <- 1972L
  x$..DayteTime <- dtt_floor(x$..DayteTime, units = units)

  colnames(data) <- c("..DayteTime", "..Value")

  if(as.Date("1972-02-28") %in% dtt_date(data$..DayteTime) &
     !as.Date("1972-02-29") %in% dtt_date(data$..DayteTime)) {
    dtt_days(x$..DayteTime[dtt_months(x$..DayteTime) == 2L & dtt_days(x$..DayteTime) == 29L]) <- 28L 
  }
  x2 <- merge(x, data, by = "..DayteTime", all.x = TRUE, sort = FALSE)
  x[[value]][which] <- x2$..Value[which]
  x$..DayteTime <- NULL
  x
}

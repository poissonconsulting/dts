#' Completes the date_time column of a data frame into its component parts.
#'
#' @param x A data frame with a Date or POSIXct column.
#' @inheritParams check_dts
#' @param year A flag indicating whether to add a Year column.
#' @param month A flag indicating whether to add a Month column.
#' @param day A flag indicating whether to add a Day column.
#' @param dayte A flag indicating whether to add a Dayte column.
#' @param doy A flag indicating whether to add a Doy column.
#' @param prefix A string to add to the start of the column names.
#' @param suffix A string to add to the end of the column names.
#'
#' @return The updated data frame.
#' @export
#'
#' @examples
#' dts_separate_date(dts_data[c(1,3),])
dts_separate_date <- function(
  x, date_time = "DateTime", year = TRUE, month = TRUE, day = TRUE, dayte = FALSE,
  doy = FALSE, prefix = "", suffix = "") {
  
  check_string(date_time)
  check_data(x, date_time)
  
  check_flag(year)
  check_flag(month)
  check_flag(day)
  check_flag(dayte)
  check_flag(doy)
  check_string(prefix)
  check_string(suffix)
  
  checkor(
    check_vector(
      x[[date_time]], Sys.Date(), x_name = 
        paste0("column '", date_time, "' of x")),
    check_vector(
      x[[date_time]], Sys.time(), x_name = 
        paste0("column '", date_time, "' of x"))
  )
  
  if(year) x[[paste0(suffix, "Year", prefix)]] <- dtt_year(x[[date_time]])
  if(month) x[[paste0(suffix, "Month", prefix)]] <- dtt_month(x[[date_time]])
  if(day) x[[paste0(suffix, "Day", prefix)]] <- dtt_day(x[[date_time]])
  if(dayte) x[[paste0(suffix, "Dayte", prefix)]] <- dtt_dayte(x[[date_time]])
  if(doy) x[[paste0(suffix, "Doy", prefix)]] <- dtt_doy(x[[date_time]])
  
  x
}

#' Completes the date_time column of a data frame into its component parts.
#'
#' @param x A data frame with a Date or POSIXct column.
#' @inheritParams check_dts
#' @inheritParams dts_separate_date
#' @param hour A flag indicating whether to add a Hour column.
#' @param minute A flag indicating whether to add a Minute column.
#' @param second A flag indicating whether to add a Second column.
#' @param date A flag indicating whether to add a Date column.
#' @param dayte_time A flag indicating whether to add a DayteTime column.
#'
#' @return The updated data frame.
#' @export
#'
#' @examples
#' dts_separate_date_time(dts_data[c(1,3),])
dts_separate_date_time <- function(
  x, date_time = "DateTime", year = TRUE, month = TRUE, day = TRUE, 
  hour = TRUE, minute = TRUE, second = TRUE, date = FALSE, dayte = FALSE,
  doy = FALSE, dayte_time = FALSE, 
  prefix = "", suffix = "") {
  
  check_string(date_time)
  check_data(x, date_time)
  
  check_flag(year)
  check_flag(month)
  check_flag(day)
  check_flag(hour)
  check_flag(minute)
  check_flag(second)
  check_flag(dayte_time)
  
  check_string(prefix)
  check_string(suffix)
  
  x <- dts_separate_date(x, date_time = date_time,
                         year = year, month = month, day = day,
                         prefix = prefix, suffix = suffix)
  
  if(hour) x[[paste0(suffix, "Hour", prefix)]] <- dtt_hour(x[[date_time]])
  if(minute) x[[paste0(suffix, "Minute", prefix)]] <- dtt_minute(x[[date_time]])
  if(second) x[[paste0(suffix, "Second", prefix)]] <- dtt_second(x[[date_time]])
  if(date) x[[paste0(suffix, "Date", prefix)]] <- dtt_date(x[[date_time]])
  
  if(dayte || doy)
    x <- dts_separate_date(x, date_time = date_time,
                           year = FALSE, month = FALSE, day = FALSE,
                           dayte = dayte, doy = doy,
                           prefix = prefix, suffix = suffix)
  
  if(dayte_time) x[[paste0(suffix, "DayteTime", prefix)]] <- 
    dtt_dayte_time(x[[date_time]])
  
  x
}

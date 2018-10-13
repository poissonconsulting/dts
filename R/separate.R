#' Separates the Date or POSIXct column of a data frame into its component parts.
#'
#' @param x A data frame with a Date or POSIXct column.
#' @inheritParams check_dts
#' @param year A flag indicating whether to add a Year column.
#' @param month A flag indicating whether to add a Month column.
#' @param day A flag indicating whether to add a Day column.
#' @param hour A flag indicating whether to add a Hour column.
#' @param minute A flag indicating whether to add a Minute column.
#' @param second A flag indicating whether to add a Second column.
#' @param dayte A flag indicating whether to add a Dayte column.
#' @param prefix A string to add to the start of the column names.
#' @param suffix A string to add to the end of the column names.
#'
#' @return The updated data frame.
#' @export
#'
#' @examples
#' dts_separate(dts_data[c(1,3),])
dts_separate <- function(
  x, dtt = "DateTime", year = TRUE, month = TRUE, day = TRUE, 
  hour = FALSE, minute = FALSE, second = FALSE, dayte = TRUE,
  prefix = "", suffix = "") {
  
  check_dts(x, dtt)

  check_flag(year)
  check_flag(month)
  check_flag(day)
  check_flag(hour)
  check_flag(minute)
  check_flag(second)
  check_flag(dayte)
  check_string(prefix)
  check_string(suffix)

  if(year) x[[paste0(suffix, "Year", prefix)]] <- dtt_year(x[[dtt]])
  if(month) x[[paste0(suffix, "Month", prefix)]] <- dtt_month(x[[dtt]])
  if(day) x[[paste0(suffix, "Day", prefix)]] <- dtt_day(x[[dtt]])
  if(hour) x[[paste0(suffix, "Hour", prefix)]] <- dtt_hour(x[[dtt]])
  if(minute) x[[paste0(suffix, "Minute", prefix)]] <- dtt_minute(x[[dtt]])
  if(second) x[[paste0(suffix, "Second", prefix)]] <- dtt_second(x[[dtt]])
  if(dayte) x[[paste0(suffix, "Dayte", prefix)]] <- dtt_dayte(x[[dtt]])
  x
}

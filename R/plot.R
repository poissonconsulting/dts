#' Plot DateTime Series
#'
#' @param x A data frame with
#' @param date_time A string specifying the column with the POSIXct values
#' @param value A string specifying the column with the time series values
#'
#' @return A ggplot object
#' @export
dts_plot <- function(x, date_time = "DateTime", value = "Value") {
  requireNamespace("ggplot2")
  check_string(value)
  check_dts(x, date_time, value)
  
  ggplot2::ggplot(data = x, ggplot2::aes_string(x = date_time, y = value)) +
    ggplot2::geom_line()
}

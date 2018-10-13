#' Plot DateTime Series
#'
#' @param x A data frame with
#' @param date_time A string specifying the column with the POSIXct values
#' @param colname A string specifying the column with the time series values
#'
#' @return A ggplot object
#' @export
dts_plot <- function(x, date_time = "DateTime", colname = "Value") {
  requireNamespace("ggplot2")
  check_string(colname)
  check_dts(x, date_time, colname)
  
  ggplot2::ggplot(data = x, ggplot2::aes_string(x = date_time, y = colname)) +
    ggplot2::geom_line()
}

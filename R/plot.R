#' Plot DateTime Series
#'
#' @param x A data frame with
#' @param dtt A string specifying the column with the POSIXct values
#' @param colname A string specifying the column with the time series values
#'
#' @return A ggplot object
#' @export
dts_plot <- function(x, dtt = "DateTime", colname = "Value") {
  requireNamespace("ggplot2")
  chk_string(colname)
  check_dts(x, dtt, colname)
  
  ggplot2::ggplot(data = x, ggplot2::aes_string(x = dtt, y = colname)) +
    ggplot2::geom_line()
}

#' DateTime Data Frame Column Names
#'
#' @inheritParams check_dts
#' @return A character vector of the other column names.
#' @export
#'
#' @examples
#' dts_colnames(dts_data)
dts_colnames <- function(x, date_time = "DateTime") {
  check_dts(x, date_time)
  colnames <- colnames(x)
  colnames <- setdiff(colnames, date_time)
  colnames
}

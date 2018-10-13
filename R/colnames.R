#' DateTime Data Frame Column Names
#'
#' @inheritParams check_dts
#' @return A character vector of the other column names.
#' @export
#'
#' @examples
#' dts_colnames(dts_data)
dts_colnames <- function(x, dtt = "DateTime") {
  check_dts(x, dtt)
  colnames <- colnames(x)
  colnames <- setdiff(colnames, dtt)
  colnames
}

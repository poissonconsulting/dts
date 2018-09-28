#' As Tibble
#' 
#' Conditionally converts a data frame to a tibble if the tibble package is installed.
#'
#' @param x A data frame to coerce to a tibble.
#' @param require A flag indicating whether to require coercion.
#' @return If possible a tibble, otherwise a data frame.
#' @export
#' @examples 
#' dts_as_tibble(data.frame())
dts_as_tibble <- function(x, require = FALSE) {
  check_data(x)
  check_flag(require)
  if(requireNamespace("tibble", quietly = !require))
    x <- tibble::as_tibble(x)
  x
}

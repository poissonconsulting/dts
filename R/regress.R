#' Regress
#'
#' Fill in missing values by linear regression.
#' 
#' @inheritParams check_dts
#' @param predictor A string of the double column to use as a predictor variable.
#' @param min_gap An integer of the minimum gap to leave between regressed and existing values.
#' @param intercept A flag indicating whether an intercept should be estimated (or fixed at 0)
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_regress(dts_data[c(1,2,3),])
dts_regress <- function(x, date_time = "DateTime", value = "Value", predictor = "Value2",
                              intercept = TRUE, min_gap = 10L) {
  check_dts(x, date_time = date_time, value = value, sorted = TRUE, 
            complete = TRUE, key = date_time)

  x
}

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
  check_dts(x, date_time = date_time, value = predictor)
  if(identical(value, predictor)) 
    err("value column '", value, "' is also predictor column")
  
  if (!nrow(x) || min_gap > nrow(x)) 
    return(x)
  
  # gap <- size_gaps(is.na(x[[value]]))
  # xout <- which(gap > min_gap)
  # xout <- intersect(xout, !is.na(x[[predictor]]))
  # 
  # if(length(xout) < 3) {
  #   return(x)
  # }
  # 
  # 
  # check_flag(intercept)
  # min_gap <- check_count(min_gap, coerce = TRUE)

  x
}

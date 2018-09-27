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
#' dts_regress(dts_data, min_gap = 0L)[1:5,]
dts_regress <- function(x, date_time = "DateTime", value = "Value", predictor = "Value2",
                        intercept = TRUE, min_gap = 10L) {
  check_dts(x, date_time = date_time, value = value, sorted = TRUE, 
            complete = TRUE, key = date_time)
  check_dts(x, date_time = date_time, value = predictor)
  if(identical(value, predictor)) 
    err("value column '", value, "' is also predictor column")
  check_flag(intercept)
  check_count(min_gap)
  
  which <- which_replace(x[[value]], min_gap = min_gap)
  
  if(!length(which)) return(x)
  
  data <- x[!is.na(x[[value]]) & !is.na(x[[predictor]]),]
  if(nrow(data) < 4L)
    err("there are less than 4 values to fit the regression")

  formula <- if(intercept) y ~ x else y ~ x - 1

  data <- x[c(value, predictor)]
  names(data) <- c("y", "x")
  
  mod <- stats::lm(formula, data = data)
  newdata <- data.frame(x = x[[predictor]][which])
  x[[value]][which] <- stats::predict(mod, newdata = newdata)
  x
}

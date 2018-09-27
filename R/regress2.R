#' Regress2
#'
#' Fill in missing values by multiple regression.
#' 
#' @inheritParams check_dts
#' @param predictor A string of the double column to use as a predictor variable.
#' @param predictor2 A string of the double column to use as the second predictor variable.
#' @param min_gap An integer of the minimum gap to leave between regressed and existing values.
#' @param intercept A flag indicating whether an intercept should be estimated (or fixed at 0)
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_regress2(dts_data, min_gap = 0L)[1:5,]
dts_regress2 <- function(x, date_time = "DateTime", value = "Value", 
                        predictor = "Value2", predictor2 = "Value3",
                        intercept = TRUE, min_gap = 10L) {
  check_dts(x, date_time = date_time, value = value, sorted = TRUE, 
            complete = TRUE, key = date_time)
  check_dts(x, date_time = date_time, value = predictor)
  check_dts(x, date_time = date_time, value = predictor2)
  if(identical(value, predictor)) 
    err("value column '", value, "' is also predictor column")
  if(identical(value, predictor2)) 
    err("value column '", value, "' is also predictor2 column")
  if(identical(predictor, predictor2)) 
    err("predictor column '", value, "' is also predictor2 column")
  
  check_flag(intercept)
  check_count(min_gap)
  
  which <- which_replace(x[[value]], min_gap = min_gap)
  
  if(!length(which)) return(x)
  
  data <- x[!is.na(x[[value]]) & !is.na(x[[predictor]]) & !is.na(x[[predictor2]]),]
  if(nrow(data) < 5L)
    err("there are less than 5 values to fit the regression")

  formula <- if(intercept) y ~ x + x2 else y ~ x + x2 - 1

  data <- x[c(value, predictor, predictor2)]
  names(data) <- c("y", "x", "x2")
  
  mod <- stats::lm(formula, data = data)
  newdata <- data.frame(x = x[[predictor]], x2 = x[[predictor2]])[which,]
  x[[value]][which] <- stats::predict(mod, newdata = newdata)
  x
}
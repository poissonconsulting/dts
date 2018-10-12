#' Regress2
#'
#' Fill in missing values by multiple regression.
#' 
#' @inheritParams check_dts
#' @inheritParams dts_regress
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_regress2(dts_data)[1:5,]
dts_regress2 <- function(x, date_time = "DateTime", 
                         value = c("Value", "Value2", "Value3"),
                        intercept = TRUE, min_gap = 0L, min_n = 5L) {
  check_vector(value, "", length = 3L)
  check_dts(x, date_time = date_time, value = value)
  check_flag(intercept)
  check_count(min_gap)
  check_scalar(min_n, c(5L, .Machine$integer.max))
  
  which <- which_replace(x[[value[1]]], min_gap = min_gap)
  
  if(!length(which)) return(x)
  
  data <- x[!is.na(x[[value[1]]]) & !is.na(x[[value[2]]]) & !is.na(x[[value[3]]]),]
  if(nrow(data) < min_n)
    err(cn(min_n, "there %r less than %n value%s to fit the regression"))

  formula <- if(intercept) y ~ x + x2 else y ~ x + x2 - 1

  data <- x[value]
  names(data) <- c("y", "x", "x2")
  
  mod <- stats::lm(formula, data = data)
  newdata <- data.frame(x = x[[value[2]]], x2 = x[[value[3]]])[which,]
  x[[value[1]]][which] <- stats::predict(mod, newdata = newdata)
  x
}

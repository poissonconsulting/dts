#' Regress
#'
#' Fill in missing values by linear regression.
#' 
#' @inheritParams check_dts
#' @param min_gap An integer of the minimum gap to leave between regressed and existing values.
#' @param intercept A flag indicating whether an intercept should be estimated (or fixed at 0)
#' @param min_n A count of the minimum of data points to require.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dts_regress(dts_data)[1:5,]
dts_regress <- function(x, dtt = "DateTime", colname = c("Value", "Value2"),
                        intercept = TRUE, min_gap = 0L, min_n = 4L) {
  check_vector(colname, "", length = 2L)
  check_dts(x, dtt = dtt, colname = colname)
  check_flag(intercept)
  check_count(min_gap)
  check_scalar(min_n, c(4L, .Machine$integer.max))
  
  which <- which_replace(x[[colname[1]]], min_gap = min_gap)
  
  if(!length(which)) return(x)
  
  data <- x[!is.na(x[[colname[1]]]) & !is.na(x[[colname[2]]]),]
  if(nrow(data) < min_n)
    err(cn(min_n, "there %r less than %n value%s to fit the regression"))

  formula <- if(intercept) y ~ x else y ~ x - 1

  data <- x[colname]
  names(data) <- c("y", "x")
  
  mod <- stats::lm(formula, data = data)
  newdata <- data.frame(x = x[[colname[2]]][which])
  x[[colname[1]]][which] <- stats::predict(mod, newdata = newdata)
  x
}

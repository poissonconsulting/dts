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
dts_regress2 <- function(x, dtt = "DateTime", 
                         colname = c("Value", "Value2", "Value3"),
                        intercept = TRUE, min_gap = 0L, min_n = 5L) {
  chk_vector(colname)
  check_values(colname, "")
  check_dim(colname, values = 3L)
  check_dts(x, dtt = dtt, colname = colname)
  chk_flag(intercept)
  chk_whole_number(min_gap)
  chk_gte(min_gap)
  chk_scalar(min_n)
  chk_gte(min_n)
  
  which <- which_replace(x[[colname[1]]], min_gap = min_gap)
  
  if(!length(which)) return(x)
  
  data <- x[!is.na(x[[colname[1]]]) & !is.na(x[[colname[2]]]) & !is.na(x[[colname[3]]]),]
  if(nrow(data) < min_n)
    err(cn(min_n, "there %r less than %n value%s to fit the regression"))

  formula <- if(intercept) y ~ x + x2 else y ~ x + x2 - 1

  data <- x[colname]
  names(data) <- c("y", "x", "x2")
  
  mod <- stats::lm(formula, data = data)
  newdata <- data.frame(x = x[[colname[2]]], x2 = x[[colname[3]]])[which,]
  x[[colname[1]]][which] <- stats::predict(mod, newdata = newdata)
  x
}

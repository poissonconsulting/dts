agg <- function(x, .fun, ...) {
  x[[2]] <- .fun(x[[2]], ...)
  x[1,]
}

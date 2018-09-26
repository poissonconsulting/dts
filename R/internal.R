agg <- function(x, .fun, ...) {
  x[[2]] <- .fun(x[[2]], ...)
  x[1,]
}

size_gaps <- function (x) 
{
    stopifnot(is.logical(x))
    stopifnot(all(!is.na(x)))
    if (!length(x)) 
        return(integer(0))
    gaps <- rep(0L, length(x))
    which <- which(x)
    if (!length(which)) 
        return(gaps)
    contin <- c(diff(which), 2L) == 1L
    n <- length(contin)
    gap <- rep(1L, n)
    i <- 1L
    while (i <= n) {
        if (contin[i]) {
            j <- i + 1L
            while (contin[j]) {
                j <- j + 1L
            }
            gap[i:j] <- j - i + 1L
            i <- j + 1L
        }
        else {
            i <- i + 1L
        }
    }
    gaps[which] <- gap
    gaps
}
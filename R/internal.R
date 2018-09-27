agg <- function(x, .fun, ...) {
  x[[2]] <- .fun(x[[2]], ...)
  x[1,]
}

which_replace <- function(x, max_span = .Machine$integer.max, 
                          min_gap = 0L, ends = TRUE) {
  x <- is.na(x)
  if(!any(x)) return(integer(0))
  x <- diff(c(FALSE, x, FALSE))
  df <- data.frame(start = which(x == 1))
  df$end = which(x == -1)
  
  if(!ends) {
    if(df$start[1] == 1) df <- df[-1,]
    if(df$end[nrow(df)] == length(x)) df <- df[-nrow(df),]
  }

  df <- df[df$end - df$start <=  max_span,]
  if(!nrow(df)) return(integer(0))
  
  df$start <- df$start + min_gap
  df$end <- df$end - min_gap
  df <- df[df$end - df$start > 0,]
  if(!nrow(df)) return(integer(0))

  df$end <- df$end - 1L
  which <- mapply(seq, df$start, df$end, USE.NAMES = FALSE)
  which <- unlist(which)
  which <- sort(which)
  which
}

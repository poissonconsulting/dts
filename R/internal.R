agg <- function(x, .fun, ...) {
  x[2:ncol(x)] <- lapply(x[2:ncol(x)], .fun, ...)
  x[1,]
}

dot <- function(x) paste0("..", x)

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
  
  df$start[df$start != 1] <- df$start[df$start != 1] + min_gap
  df$end[df$end != length(x)] <- df$end[df$end != length(x)] - min_gap
  df <- df[df$end - df$start > 0,]
  if(!nrow(df)) return(integer(0))

  df$end <- df$end - 1L
  which <- mapply(seq, df$start, df$end, USE.NAMES = FALSE)
  which <- unlist(which)
  which <- sort(which)
  which
}

interpolate <- function(x, max_span) {
  which <- which_replace(x, max_span = max_span, ends = FALSE)
  if(!length(which)) return(x)
  x[which] <- stats::approx(x, xout = which)$y
  x
}

extrapolate <- function(x) {
  wch <- which(!is.na(x))
  if(!length(wch)) return(x)
  wch <- wch[c(1L, length(wch))]
  x[1:wch[1]] <- x[wch[1]]
  x[wch[2]:length(x)] <- x[wch[2]]
  x
}

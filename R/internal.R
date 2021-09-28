set_na <- function(x) {
  is.na(x) <- TRUE
  x
}

calc_atu <- function(start, atu, cumsum) {
  cumsum <- cumsum - start
  wch <- which(cumsum >= atu)
  if(!length(wch)) return(0L)
  wch[1]
}

calc_atus <- function(atu, cumsum) {
  vapply(cumsum, calc_atu, 1L, atu = atu, cumsum = cumsum)
}

adjust_units <- function(x, units, unit) x * dtt_units_per_unit(units, unit)

agg <- function(x, .dts_fun, ...) {
  x[2:ncol(x)] <- lapply(x[2:ncol(x)], .dts_fun, ...)
  x[1,]
}

delay <- function(x, delay) {
  n <- length(x)
  if(!n || delay == 0) return(x)
  na <- x[1]
  is.na(na) <- TRUE
  if(abs(delay) >= n) return(rep(na, n))
  if(delay > 0) return(c(rep(na, delay), x[1:(n-delay)]))
  delay <- abs(delay)
  return(c(x[(1 + delay):n], rep(na, delay)))
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

extrapolate <- function(x, max_span) {
  wch <- which(!is.na(x))
  if(!length(wch)) return(x)
  wch <- wch[c(1L, length(wch))]
  start_value <- x[wch[1]]
  end_value <- x[wch[2]]
  start_indices <- 1:wch[1]
  end_indices <- wch[2]:length(x)
  
  nstart <- length(start_indices)
  nend <- length(end_indices)
  
  start_indices <- start_indices[(max(1, nstart - max_span)):nstart]
  end_indices <- end_indices[1:(min(max_span + 1, nend))]
  
  x[start_indices] <- start_value
  x[end_indices] <- end_value
  x
}

# taken from checkr pacakge since being deprecated 
# added so check_dts will work

err_deparse <- function(x) {
  if (!is.character(x)) x <- deparse(x) 
  if (isTRUE(is.na(x))) x <- "NA"
  x
}

is_count <- function(x)  is.numeric(x) && length(x) == 1 &&
  !is.na(x) && x >= 0 && identical(as.numeric(x), floor(x))

is_count_range <- function(x) is.numeric(x) && length(x) %in% 1:2 && 
  all(!is.na(x) & x >= 0 & identical(as.numeric(x), floor(x)))

is_count_vector <- function(x) is.numeric(x) && length(x) >= 3 && 
  all(!is.na(x) & x >= 0 & identical(as.numeric(x), floor(x)))

is_string <- function(x)  (is.character(x) || is.factor(x)) && length(x) == 1 && !is.na(x)

is_flag <- function(x)  is.logical(x) && length(x) == 1 && !is.na(x)

is_NA <- function(x)  length(x) == 1 && is.na(x)

is_length <- function(x) is_flag(x) || is_NA(x) || is_count_range(x) || is_count_vector(x)

co_and <- function(object, 
                   one = "%o has %n value%s: %c", 
                   object_name = substitute(object)) {
  object_name <- err_deparse(object_name)
  err::co(object, one = one, conjunction = "and", object_name = object_name)
}

cc_and <- function(object) {
  err::cc(object, conjunction = "and")
}

cc_or <- function(object) {
  err::cc(object, conjunction = "or")
}

check_nas <- function(x,
                      values,
                      x_name = substitute(x),
                      error = TRUE) {
  
  x_name <- chk_deparse(x_name)
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  if(!length(values)) return(invisible(x))
  
  nas <- is.na(values)
  
  if(!length(nas)) return(invisible(x))
  
  if(!any(nas) && any(is.na(x))) {
    chk_fail(x_name, " must not include missing values", error = error)
  } else if (all(nas) && !all(is.na(x))) {
    chk_fail(x_name, " must only include missing values", error = error)
  }
  invisible(x)
}

chk_deparse <- function(x) {
  if (!is.character(x)) x <- deparse(x)
  if (isTRUE(is.na(x))) x <- "NA"
  if (!is_string(x)) err(substitute(x), " must be a string")
  x
}

chk_fail <- function(..., error) {
  if (missing(error) || isTRUE(error)) err(...)
  wrn(...)
}

check_inherits <- function(x, class,
                           x_name = substitute(x),
                           error = TRUE) {
  x_name <- chk_deparse(x_name)
  
  if (!is_string(class)) err(substitute(class), " must be a string")
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  if(!inherits(x, class)) {
    chk_fail(x_name, " must inherit from class ", class, error = error)
  }
  invisible(x)
}

check_colnames <- function(x, 
                           colnames = character(0), 
                           exclusive = FALSE, 
                           order = FALSE,
                           x_name = substitute(x),
                           error = TRUE) {
  
  x_name <- chk_deparse(x_name)
  
  check_vector(colnames, "", unique = TRUE)
  
  if (!is_flag(exclusive)) err(substitute(exclusive), " must be a flag")
  if (!is_flag(order)) err(substitute(order), " must be a flag")
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  names(colnames) <- NULL
  x_colnames <- colnames(x)
  
  if(is.null(x_colnames)) chk_fail(x_name, 
                                   " must have column names", 
                                   error = error)
  
  if(!length(colnames)) {
    if(exclusive && length(x_colnames))
      chk_fail(x_name, " must not have any columns", error = error)
    return(x)
  }
  
  if (exclusive) {
    if (order) {
      if (!identical(x_colnames, colnames))
        chk_fail(
          x_name, 
          " column names must be identical to ", 
          cc_and(colnames), 
          error = error
        )
    } else {
      if (!identical(sort(x_colnames), sort(colnames)))
        chk_fail(
          x_name, 
          " column names must include and only include ", 
          cc_and(colnames), 
          error = error
        )
    }
  } else {
    x_colnames <- x_colnames[x_colnames %in% colnames]
    if (order) {
      if (!identical(x_colnames, colnames))
        chk_fail(
          x_name, 
          " column names must include ", 
          cc_and(colnames), 
          " in that order", 
          error = error)
    } else {
      if (!identical(sort(x_colnames), sort(colnames)))
        chk_fail(
          x_name, 
          " column names must include ", 
          cc_and(colnames), 
          error = error
        )
    }
  }
  invisible(x)
}

check_class_internal <- function(x,
                                 values,
                                 x_name = substitute(x),
                                 error = TRUE) {
  x_name <- chk_deparse(x_name)
  
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  for(class in class(values)) {
    if (!inherits(x, class)) {
      chk_fail(x_name, " must be class ", class, error = error)
    }
  }
  if(!identical(class(values), intersect(class(x), class(values)))) {
    chk_fail(x_name, " must inherit from classes ", cc_and(class(values)), 
             " in that order", error = error)
  }
  if("units" %in% class(values)) {
    if(!requireNamespace("units", quietly = TRUE))
      err("package 'units' is required to check units")
    if(!identical(units::deparse_unit(x), units::deparse_unit(values)))
      err(
        x_name, 
        " must have units '", 
        units::deparse_unit(values), 
        "' not '", 
        units::deparse_unit(x), 
        "'"
      )
  }
  
  invisible(x)
}

check_values <- function(x, values,
                         only = FALSE,
                         x_name = substitute(x),
                         error = TRUE) {
  x_name <- chk_deparse(x_name)
  
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  if (!is.atomic(x)) err(x_name, " must be an atomic vector")
  if (!is.atomic(values)) err("values must be an atomic vector")
  
  check_class_internal(x, values, x_name = x_name, error = error)
  check_nas(x, values, x_name = x_name, error = error)
  
  x_nona <- x[!is.na(x)]
  if(!length(x_nona)) return(invisible(x))
  
  values <- values[!is.na(values)]
  if(!only && length(values) < 2) return(invisible(x))
  
  x_nona <- sort(x_nona)
  values <- sort(values)
  
  if(!only && identical(length(values), 2L)) {
    if(x_nona[1] < values[1] || x_nona[length(x_nona)] > values[2]) {
      chk_fail("the values in ", x_name,
               " must lie between ", cc_and(values[1:2]), error = error)
    }
  } else if (!all(x_nona %in% values)) {
    unpermitted <- x_nona[!x_nona %in% values]
    unpermitted <- unique(unpermitted)
    values <- unique(values)
    if(length(values) < 10) {
      chk_fail(
        x_name, 
        " can only include values ", 
        cc_or(values), 
        error = error
      )
    } else if(length(unpermitted) < 10) {
      chk_fail(
        x_name, 
        " has unpermitted values ", 
        cc_and(unpermitted), 
        error = error
      )
    } else
      chk_fail(x_name, " has unpermitted values ", error = error)
  }
  invisible(x)
}

check_n <- function(x, n, range, x_name, n_name, error) {
  if(is_NA(range)) return(x)
  
  if(is.null(n)) n <- 0L
  
  if(is_flag(range) && range) {
    range <- c(1L, .Machine$integer.max)
  } else if(is_flag(range) && !range)
    range <- 0L
  
  if (identical(length(range), 1L)) {
    if (any(n != range)) {
      chk_fail(
        x_name, 
        " must have ", 
        range, 
        " ", 
        n_name, 
        cn(range, "%s"), 
        error = error
      )
    }
    return(x)
  }
  if(identical(length(range), 2L)) {
    if (any(n < min(range))) {
      chk_fail(
        x_name, 
        " must have at least ", 
        min(range), 
        " ", 
        n_name, 
        cn(min(range), "%s"), 
        error = error
      )
    }
    if (any(n > max(range))) {
      chk_fail(
        x_name, 
        " must not have more than ", 
        max(range), 
        " ", n_name, 
        cn(max(range), "%s"), 
        error = error
      )
    }
    return(x)
  }
  range <- sort(unique(range))
  if(!n %in% range) {
    chk_fail(x_name, " must have ", cc_or(range), " ", n_name, "s", error = error)
  }
  return(x)
}

check_nrow <- function(x, nrow = TRUE,
                       x_name = substitute(x),
                       error = TRUE) {

  x_name <- chk_deparse(x_name)
  
  if(!is_length(nrow))
    err(
      substitute(nrow), 
      " must be a flag, a missing value, a count, a count range or a count vector"
    )
  
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  check_n(
    x, 
    n = nrow(x), 
    range = nrow, 
    x_name = x_name, 
    n_name = "row", 
    error = error
  )
  invisible(x)
}

check_nchar <- function(x, 
                        nchar = TRUE, 
                        x_name = substitute(x),
                        error = TRUE) {
  x_name <- chk_deparse(x_name)
  
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  if(!(is_flag(nchar) || is_NA(nchar) || is_count(nchar) || is_count_range(nchar)))
    err("nchar must be a flag, NA, count or count range")
  
  check_n(
    x, 
    n = nchar(x), 
    range = nchar, 
    x_name = x_name, 
    n_name = "character", 
    error = error
  )
  
  invisible(x)
}

check_grepl <- function(x, 
                        pattern = ".*",
                        regex = pattern, 
                        x_name = substitute(x),
                        error = TRUE) {
  x_name <- chk_deparse(x_name)
  
  chk::chk_string(pattern)
  if (!is_flag(x)) err(substitute(x), " must be a flag")
  
  if (!missing(regex)) {
    chk::chk_string(regex)
    pattern <- regex
  }
  
  if(!all(grepl(pattern, x)))
    chk_fail(
      x_name, 
      " must match regular expression '", 
      pattern, 
      "'", 
      error = error
    )
  invisible(x)
}

check_named <- function(x, nchar = c(0L, .Machine$integer.max), 
                        pattern = ".*",
                        regex = pattern, unique = FALSE, 
                        x_name = substitute(x),
                        error = TRUE) {
  x_name <- chk_deparse(x_name)
  
  chk::chk_flag(unique)
  if (!is_flag(error)) err(substitute(error), " must be a flag")

  if (!missing(regex)) {
    chk::chk_string(regex)
    pattern <- regex
  }
  
  if(is.null(names(x))) {
    chk_fail(x_name, " must be named", error = error)
  } else {
    check_nchar(
      names(x), 
      nchar = nchar, 
      x_name = paste("names of", x_name), 
      error = error
    )
    check_grepl(
      names(x), 
      pattern = pattern,
      x_name = paste("names of", x_name), 
      error = error
    )
    if(unique)
      check_unique(names(x), x_name = paste("names of", x_name), error = error)
  }
  invisible(x)
}

check_unnamed <- function(x,
                          x_name = substitute(x),
                          error = TRUE) {
  x_name <- chk_deparse(x_name)
  
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  if(!is.null(names(x))) {
    chk_fail(x_name, " must be unnamed", error = error)
  }
  invisible(x)
}

check_unique <- function(x,
                         x_name = substitute(x),
                         error = TRUE) {
  x_name <- chk_deparse(x_name)
  
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  if(anyDuplicated(x)) {
    chk_fail(x_name, " must be unique", error = error)
  }
  invisible(x)
}

check_sorted <- function(x,
                         x_name = substitute(x),
                         error = TRUE) {
  x_name <- chk_deparse(x_name)
  
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  is_unsorted <- is.unsorted(x, na.rm = TRUE)
  if (is.na(is_unsorted) || is_unsorted) {
    chk_fail(x_name, " must be sorted", error = error)
  }
  invisible(x)
}

check_no_attributes <- function(x,
                                names = TRUE,
                                class = TRUE,
                                x_name = substitute(x),
                                error = TRUE) {

  x_name <- chk_deparse(x_name)
  
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  attr <- attributes(x)
  if(!class) attr$class <- NULL
  if(!names) attr$names <- NULL
  if(length(attr)) chk_fail(x_name, " must not have attributes", error = error)
  
  invisible(x)
}

check_vector <- function(x,
                         values = NULL,
                         length = NA,
                         unique = FALSE,
                         sorted = FALSE,
                         named = NA,
                         attributes = named,
                         names = TRUE,
                         class = TRUE,
                         only = FALSE,
                         x_name = substitute(x),
                         error = TRUE) {
  
  x_name <- chk_deparse(x_name)
  
  if (!is_flag(unique)) err(substitute(unique), " must be a flag")
  if (!is_flag(sorted)) err(substitute(sorted), " must be a flag")
  if (!is_flag(names)) err(substitute(names), " must be a flag")
  
  if(!(is_flag(named) || is_NA(named))) 
    err("named must be a flag or NA")
  
  if(!(is_flag(attributes) || is_NA(attributes))) 
    err("attributes must be a flag or NA")
  
  if(!is_NA(named) && named && !is_NA(attributes) && !attributes && names)
    err("names are attributes")
  
  if (!is_flag(only)) err(substitute(only), " must be a flag")
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  if (!is.atomic(x)) err(x_name, " must be an atomic vector")
  
  check_length(x, length = length, x_name = x_name, error = error)
  
  if(!is.null(values)) {
    check_values(x, values = values, only = only, 
                 x_name = x_name, error = error)
  } else if(only)
    wrn("only is TRUE but values is undefined")
  
  if(unique) check_unique(x, x_name = x_name, error = error)
  if(sorted) check_sorted(x, x_name = x_name, error = error)
  
  if(is_flag(named) && named) {
    check_named(x, x_name = x_name, error = error)
  } else if(is_flag(named) && !named)
    check_unnamed(x, x_name = x_name, error = error)
  
  if(is_flag(attributes) && attributes) {
    check_attributes(
      x, 
      names = names, 
      class = class, 
      x_name = x_name, 
      error = error
    )
  } else if(is_flag(attributes) && !attributes) {
    check_no_attributes(
      x, 
      names = names, 
      class = class, 
      x_name = x_name, 
      error = error
    )
  }
  
  invisible(x)
}

check_names <- function(x, 
                        names = character(0), 
                        exclusive = FALSE, 
                        order = FALSE,
                        unique = FALSE, 
                        complete = TRUE,
                        x_name = substitute(x),
                        error = TRUE) {
  x_name <- chk_deparse(x_name)
  
  if (!is_flag(unique)) err(substitute(unique), " must be a flag")
  check_vector(names, "", unique = unique)
  if (!is_flag(exclusive)) err(substitute(exclusive), " must be a flag")
  if (!is_flag(order)) err(substitute(order), " must be a flag")
  if (!is_flag(complete)) err(substitute(complete), " must be a flag")
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  check_named(x, x_name = x_name, unique = unique, error = error)
  
  names(names) <- NULL
  x_names <- names(x)
  
  if(!length(names)) {
    if(exclusive && length(x_names))
      chk_fail(x_name, " must not have any elements", error = error)
    return(x)
  }
  
  if (complete && length(setdiff(names, x_names)))
    chk_fail(
      x_name, 
      " names must include ", 
      cc_and(setdiff(names, x_names)) , 
      error = error
    )
  
  if (exclusive && length(setdiff(x_names, names)))
    chk_fail(
      x_name, 
      " names must not include ", 
      cc_or(setdiff(x_names, names)) , 
      error = error
    )
  
  if(order && !identical(intersect(names, x_names), intersect(x_names, names)))
    chk_fail(
      x_name, 
      " names must include ", 
      cc_and(names), 
      " in that order", 
      error = error
    )
  
  invisible(x)
}

check_length <- function(x,
                         length = TRUE,
                         x_name = substitute(x),
                         error = TRUE) {
  x_name <- chk_deparse(x_name)
  
  if(!is_length(length))
    err(
      substitute(length), 
      " must be a flag, a missing value, a count, a count range or a count vector"
    )
  
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  check_n(
    x, 
    n = length(x), 
    range = length, 
    x_name = x_name, 
    n_name = "element", 
    error = error
  )
  invisible(x)
}

check_list <- function(x,
                       values = NULL,
                       length = NA,
                       unique = FALSE,
                       named = NA,
                       exclusive = FALSE,
                       order = FALSE,
                       x_name = substitute(x),
                       error = TRUE) {
  x_name <- chk_deparse(x_name)
  
  if (!is.list(x)) err(x_name, " must be a list")
  
  if (!is_flag(unique)) err(substitute(unique), " must be a flag")
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  if(!(is_flag(named) || is_string(named) || is_NA(named) || is_count(named) || is_count_range(named))) 
    err("named must be a flag, string, count, count range or NA")
  
  regex <- ".*"
  nchar <- c(0L, .Machine$integer.max)
  if(is_string(named)) {
    regex <- named
    named <- TRUE
  } else if(is_count(named) || is_count_range(named)) {
    nchar <- named
    named <- TRUE
  }
  
  if(!is.null(values)) {
    if(is.list(values)) {
      check_named(values, unique = TRUE)
      check_names(x, names = names(values), exclusive = exclusive, order = order)
      
      for(name in names(values)) {
        check_values(
          x[[name]], 
          values[[name]], 
          x_name = paste("element", name, "of", x_name), 
          error = error
        )
      }
    } else {
      if(!is.atomic(values)) err("values must be an atomic vector or a named list")
      check_names(x, names = values, exclusive = exclusive, order = order)
    }
  }
  check_length(x, length = length, x_name = x_name, error = error)
  
  if(unique) check_unique(x, x_name = x_name, error = error)
  
  if(is_flag(named) && named) {
    check_named(x, nchar = nchar, pattern = regex, x_name = x_name, error = error)
  } else if(is_flag(named) && !named)
    check_unnamed(x, x_name = x_name, error = error)
  
  invisible(x)
}

check_attributes <- function(x,
                             values = NULL,
                             exclusive = FALSE,
                             order = FALSE,
                             names = TRUE,
                             class = TRUE,
                             x_name = substitute(x),
                             error = TRUE) {

  x_name <- chk_deparse(x_name)
  
  if (!is_flag(names)) err(substitute(names), " must be a flag")
  if (!is_flag(class)) err(substitute(class), " must be a flag")
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  attr <- attributes(x)
  
  if(!class) attr$class <- NULL
  if(!names) attr$names <- NULL
  
  if(!length(attr)) chk_fail(x_name, " must have attributes", error = error)
  
  if(!is.null(attr) && !is.null(values)) {
    check_list(attr, values = values, order = order, exclusive = exclusive, 
               x_name = paste("attributes of", x_name), error = error)
  }
  
  invisible(x)
}

# from https://stevenmortimer.com/the-unfinished-duplicated-function/
new_duplicated <- function(x, incomparables = FALSE, fromLast = FALSE, ...) {
  
  if(!identical(incomparables, FALSE)) {
    n <- ncol(x)
    nmx <- names(x)
    nmincomparables <- names(incomparables)
    lincomparables <- length(incomparables)
    if(is.null(nmincomparables)) {
      if(lincomparables < n) {
        # pad any incomparables lists with the default value if list is shorter 
        # than the number columns in the supplied data.frame
        tmp <- c(incomparables, as.list(rep_len(FALSE, n - lincomparables)))
        names(tmp) <- nmx
        incomparables <- tmp 
      }
      if(lincomparables > n) {
        # if the list is unnamed and there are more elements in the list than 
        # there are columns, then only first n elements
        warning(paste("more columns in 'incomparables' than x, only using the first", n, "elements"))
        incomparables <- incomparables[1:n]
      }
    } else {
      # for named lists, find match, else default value
      tmp <- as.list(rep_len(FALSE, n))
      names(tmp) <- nmx
      i <- match(nmincomparables, nmx, 0L)
      if(any(i <= 0L))
        warning("not all columns named in 'incomparables' exist")
      tmp[ i[i > 0L] ] <- incomparables[i > 0L]
      incomparables <- tmp[nmx]
    }
    
    # first determine duplicates, then override when an incomparable value is 
    # found in a row since the existence of even 1 incomparable value in a row 
    # means it cannot be a duplicate
    res <- duplicated(do.call("paste", c(x, sep="\r")), fromLast = fromLast)
    
    # for better performance only bother with the columns that have incomparable 
    # values not set to the default: !identical(x, FALSE)
    run_incomp_check <- sapply(incomparables, FUN=function(x){!identical(x, FALSE)})
    if (sum(run_incomp_check) > 0L){
      incomp_check <- mapply(FUN=function(column,incomparables){match(column, incomparables)}, x[run_incomp_check], incomparables[run_incomp_check])
      # any rows with an incomparable match means, TRUE, it can override the 
      # duplicated result
      overwrite <- apply(data.frame(incomp_check), 1, function(x){any(!is.na(x))})
      res[overwrite] <- FALSE
    }
    
    return(res)
  } else if(length(x) != 1L) {
    duplicated(do.call("paste", c(x, sep="\r")), fromLast = fromLast)
  } else {
    duplicated(x[[1L]], fromLast = fromLast, ...)
  }
}

check_key <- function(x, key = names(x), na_distinct = FALSE,
                      x_name = substitute(x),
                      error = TRUE) {
  
  x_name <- chk_deparse(x_name)
  
  check_vector(key, "")
  
  if(!nrow(x)) return(x)
  
  if(!length(key)) return(invisible(x))
  
  check_colnames(x, colnames = key, x_name = x_name)
  chk::chk_flag(na_distinct)
  
  incomparables <- if(na_distinct) NA else FALSE
  if (any(new_duplicated(x[key], incomparables = incomparables))) { 
    chk_fail(co_and(key, "column%s %c in "), x_name, " must be a unique key",
             error = error)
  }
  invisible(x)
}


check_data <- function(x,
                       values = NULL,
                       nrow = NA,
                       exclusive = FALSE,
                       order = FALSE,
                       key = character(0),
                       x_name = substitute(x),
                       error = TRUE) {

  x_name <- chk_deparse(x_name)
  
  check_inherits(x, "data.frame", x_name = x_name)
  
  if (!is_flag(error)) err(substitute(error), " must be a flag")
  
  if(!is.null(values)) {
    if(is.list(values)) {
      check_colnames(x, colnames = names(values), x_name = x_name)
      check_colnames(x, colnames = names(values), x_name = x_name, 
                     exclusive = exclusive, order = order, error = error)
      
      for(name in names(values)) {
        check_values(x[[name]], values[[name]], 
                     x_name = paste("column", name, "of", x_name), error = error)
      }
    } else {
      if(!is.character(values)) err("values must be a character vector, named list or NULL")
      
      check_colnames(
        x, 
        colnames = values, 
        x_name = x_name, 
        exclusive = exclusive, 
        order = order
      )
    }
  }
  check_nrow(x, nrow = nrow, x_name = x_name, error = error)
  
  check_key(x, key = key, x_name = x_name, error = error)
  
  invisible(x)
}

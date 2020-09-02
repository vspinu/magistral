

## Factors/Categorical

#' @export
fct_lumped_levels <- function(x, prop, n, other_level = "OTHER", ignore.na = TRUE) {
  if (ignore.na)
    x <- x[!is.na(x)]
  x <- forcats::fct_lump(x, n = n, prop = prop, other_level = other_level)
  structure(levels(x), other_level = other_level)
}

#' @export
fct_set_levels <- function(x, new_levels, other_level = NULL) {
  if (is.factor(x) && identical(levels(x), new_levels)) {
    if (is.null(other_level)) return (x)
    else return(structure(x,
                          levels = structure(new_levels, other_level = .other_level),
                          class = class(x)))
  }
  if (length(unique(new_levels)) < length(new_levels))
    stop("'new_levels' contains duplicates")
  if (is.null(other_level))
    if (is.null(other_level <- attr(new_levels, "other_level")))
      other_level <- NA_integer_
  out_class <- if (is.factor(x)) class(x) else "factor"
  other_ix <-
    if (is.na(other_level)) NA_integer_
    else match(other_level, new_levels, nomatch = length(new_levels) + 1L)
  y <- match(x, new_levels, nomatch = other_ix)
  y[is.na(x)] <- NA
  .other_level <- NULL
  if (!is.na(other_level)) {
    if (other_ix > length(new_levels))
      new_levels <- c(new_levels, other_level)
    .other_level <- other_level
  }
  structure(y,
            levels = structure(new_levels, other_level = .other_level),
            class = out_class)
}

#' @export
boring_vars <- function(df, min_vals = 2, max_vals = Inf, omit_na = TRUE,
                        types = c("character", "factor")) {
  keep(names(df), function(nm) {
    if (is.null(types) || inherits(df[[nm]], types)) {
      uvals <- unique(df[[nm]])
      if (omit_na)
        uvals <- c(na.omit(uvals))
      N <- length(uvals)
      N < min_vals || max_vals < N
    } else {
      FALSE
    }
  })
}



## Prototypes

#' Extract reference value from a vector
#' @export
refval <- function(x, type = c("median", "mean", "min", "min-", "max", "max+")) {
  type <- match.arg(type)
  out <-
    if (is.character(x) || is.factor(x)) {
      switch(type,
             median = , mean = {
               tbl <- base::table(x, exclude = c(NA, NaN))
               names(tbl)[which.max(tbl)]
             },
             min = , "min-" = {
               if (is.factor(x)) levels(x)[[1]] else x[[1]]
             },
             max = , "max+" = {
               if (is.factor(x)) levels(x)[[length(levels(x))]] else x[[length(x)]]
             })
    } else {
      switch(type,
             median = median(x, na.rm = TRUE),
             mean = mean(x, na.rm = TRUE),
             min = min(x, na.rm = TRUE),
             "min-" = {
               range <- range(x, na.rm = TRUE)
               range[[1]] - (range[[2]] - range[[1]])*.01
             },
             max = max(x, na.rm = TRUE),
             "max+" = {
               range <- range(x, na.rm = TRUE)
               range[[2]] + (range[[2]] - range[[1]])*.01
             })
    }
  reclass(out, class(x), levels(x))
}

#' @rdname refval
#' @export
refval_extractor <- function(type = c("median", "mean", "min", "min-", "max", "max+")) {
  function(x, nm) {
    refval(x, type = type)
  }
}


#' @export
repval_mean <- function(x, nm) {
  out <-
    if (is.character(x) || is.factor(x))
      median(x, na.rm = T)
    else
      mean(x, na.rm = T)
  reclass(out, class(x), levels(x))
}

default_prototype_extractor <- function(x, nm) {
  NA
}

#' @export
extract_prototypes <- function(dt, nm, extractor = NULL) {
  extractor <- extractor %||% default_prototype_extractor
  prot <- dt[1, ]
  nms <- names(prot)
  for (i in seq_along(prot))
    prot[[1, i]] <- extractor(dt[[i]], nms[[i]])
  as.list(prot)
}

#' @export
fill_prototypes <- function(dt, prototype) {
  for (nm in names(prototype)) {
    x <- dt[[nm]]
    p <- prototype[[nm]]
    if (is.null(x)) {
      dt[[nm]] <- p
    } else if (is.factor(p)) {
      dt[[nm]] <- fct_set_levels(x, levels(p))
    }
  }
  dt
}

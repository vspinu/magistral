

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

#' @export
extract_prototype <- function(dt) {
  prot <- dt[1, ]
  for (i in seq_along(prot))
    prot[[1, i]] <- NA
  prot
}

#' @export
fill_prototype <- function(dt, prototype) {
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

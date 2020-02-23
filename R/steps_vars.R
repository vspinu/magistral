
#' @export
vars_remover <- function(x = NULL, regex = NULL, vars = NULL, fun = NULL, ignore.case = FALSE, ...) {
  function(x, ...) {
    vars <- c(if (!is.null(regex))
                unlist(map(regex, ~ grep(.x, names(x[["data"]]), ignore.case = ignore.case, value = TRUE))),
              if (!is.null(fun))
                fun(x[["data"]]), 
              vars)
    vars <- sort(vars)
    vars <- intersect(vars, names(x[["data"]]))
    x[["data"]] <- select(x[["data"]], -one_of(vars))
    x
  }
}
 
#' @export
vars_mem_remover <- function(x, regex = NULL, vars = NULL, fun = NULL, ignore.case = FALSE, ...) {
  vars <- sort(
    c(if (!is.null(regex))
        unlist(map(regex, ~ grep(.x, names(x[["DATA"]]), ignore.case = ignore.case, value = TRUE))),
      if (!is.null(fun))
        fun(x[["DATA"]]), 
      vars))
  function(x, ...) {
    vars <- intersect(vars, names(x[["data"]]))
    if (length(vars) > 0)
      x[["data"]] <- select(x[["data"]], -one_of(vars))
    x
  }
}

#' @export
vars_keeper <- function(x, regex = NULL, vars = NULL, fun = NULL, ignore.case = TRUE, ...) {
  function(x, ...) {
    vars <- sort(c(if (!is.null(regex))
      unlist(map(regex, ~ grep(.x, names(x[["data"]]), ignore.case = ignore.case, value = TRUE))),
      if (!is.null(fun))
        fun(x[["data"]]), 
      vars))
    x[["data"]] <- select(x[["data"]], vars)
    x
  }
}

#' @export
vars_adder <- function(x, regex = NULL, vars = NULL, fn = NULL, ignore.case = TRUE, ...) {
  function(x, ...) {
    vars <- c(
      if (!is.null(regex))
        unlist(map(regex, ~ grep(.x, names(x[["DATA"]]), ignore.case = ignore.case, value = TRUE))),
      if (!is.null(fn))
        fn(x[["data"]]), 
      vars)
    vars <- sort(vars)
    for (var in setdiff(vars, names(x[["data"]]))) {
      x[["data"]][[var]] <- x[["DATA"]][[var]]
    }
    x
  }
}

#' @export
vars_mem_adder <- function(x, regex = NULL, vars = NULL, fn = NULL, ignore.case = FALSE, ...) {
  vars <- c(
    if (!is.null(regex))
      unlist(map(regex, ~ grep(.x, names(x[["DATA"]]), ignore.case = ignore.case, value = TRUE))),
    if (!is.null(fn))
      fn(x[["data"]]), 
    vars)
  function(x, ...) {
    for (var in setdiff(vars, names(x[["data"]]))) {
      el <- x[["DATA"]][[var]]
      if (is.null(el))
        stop(sprintf("Variable '%s' is missing in DATA", var))
      x[["data"]][[var]] <- el
    }
    x
  }
}


#' @export
reclass <- function(x, class, levels) {
  if (inherits(x, class))
    x
  else
    switch(class[[1]],
           POSIXct = ,
           POSIXlt = lubridate::as_datetime(x),
           DATE = lubridate::as_date(x),
           numeric = as.numeric(x),
           integer = as.integer(x),
           logical = as.logical(x),
           character = as.character(x),
           ## FIXME: remove as.character 
           ordered = fct_set_levels(as.character(x), levels),
           factor = fct_set_levels(as.character(x), levels),
           as(x, class))
}

#' @export
vars_memoiser <- function(x, overwrite_prototypes = NULL, ...) {
  if (length(unique(names(x[["data"]]))) != ncol(x[["data"]]))
    stop("Feature names are not unique")
  prototype <- extract_prototype(x[["data"]])
  for (reg in names(overwrite_prototypes)) {
    for (nm in grep(reg, names(prototype), value = T))
      prototype[[nm]] <- overwrite_prototypes[[reg]]
  }
  function(x, verbose = TRUE, ...) {
    stopifnot(nrow(x[["data"]]) > 0)
    out <- x[["data"]]
    for (nm in names(prototype)) {
      p <- prototype[[nm]]
      v <- out[[nm]]
      if (is.null(v)) {
        if (verbose) {
          message(glue("Missing variable {nm} of class {class(p)}"))
        }
        out[[nm]] <- p
      } else {
        out[[nm]] <- reclass(v, class(p), levels(p))
      }
    }
    x[["data"]] <- select(out, !!!names(prototype))
    x
  }
}

is.factor_or_character <- function(x) {
  is.factor(x) || is.character(x)
}

#' @export
lumper.categorical <- function(x, prop = 0, ...) {
  cat_levels <-
    map(keep(x[["data"]], is.factor_or_character),
        fct_lumped_levels, prop = prop)
  function(x, ...) {
    x[["data"]] <-
      imodify(x[["data"]], function(v, nm) {
        if ((is.character(v) || is.factor(v)) & nm %in% names(cat_levels)) {
          fct_set_levels(v, cat_levels[[nm]])
        } else {
          v
        }
      })
    x
  }
}

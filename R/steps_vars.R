match_vars <- function(x, vars = NULL, regex = NULL, fn = NULL, ignore.case = FALSE, kind = "DATA", fill_names = FALSE) {
  vars <- c(if (!is.null(regex))
              unlist(lapply(regex, grep, names(x[[kind]]), ignore.case = ignore.case, value = TRUE)),
            if (!is.null(fn))
              fn(x[[kind]]), 
            vars)
  if (fill_names) {
    names <- names(vars)
    if (is.null(names)) {
      names(vars) <- vars
    } else {
      empty <- !nzchar(names)
      names[empty] <- vars[empty]
      names(vars) <- names
    }
  }
  vars
}

#' @export
select_vars <- function(df, vars) {
  if (length(vars) == 0)
    return(df)
  if (is.data.table(df)) {
    df[, vars, with = FALSE]
  } else {
    df[, vars]
  }
}

#' @export
vars_remover <- function(x = NULL, regex = NULL, vars = NULL, fn = NULL, ignore.case = FALSE, ...) {
  function(x, ...) {
    vars <- match_vars(x, vars, regex, fn, ignore.case, "data")
    vars <- setdiff(names(x[["data"]]), vars)
    x[["data"]] <- select_vars(x[["data"]], vars)
    x
  }
}
 
#' @export
vars_mem_remover <- function(x, vars = NULL, regex = NULL, fn = NULL, ignore.case = FALSE, ...) {
  vars <- sort(match_vars(x, vars, regex, fn, ignore.case, "data"))
  function(x, ...) {
    vars <- setdiff(names(x[["data"]]), vars)
    x[["data"]] <- select_vars(x[["data"]], vars)
    x
  }
}

#' @export
vars_keeper <- function(x, vars = NULL, regex = NULL, fn = NULL, ignore.case = TRUE, ...) {
  function(x, ...) {
    vars <- match_vars(x, vars, regex, fn, ignore.case, "data")
    x[["data"]] <- select_vars(x[["data"]], vars)
    x
  }
}

#' @export
vars_mem_keeper <- function(x, vars = NULL, regex = NULL, fn = NULL, ignore.case = TRUE, ...) {
  vars <- sort(match_vars(x, vars, regex, fn, ignore.case, "data"))
  function(x, ...) {
    vars <- intersect(names(x[["data"]]), vars)
    x[["data"]] <- select_vars(x[["data"]], vars)
    x
  }
}

#' @export
vars_adder <- function(x, vars = NULL, regex = NULL, fn = NULL, ignore.case = TRUE, ...) {
  function(x, ...) {
    vars <- match_vars(x, vars, regex, fn, ignore.case, "DATA", fill_names = TRUE)
    which <- match(vars, names(x[["DATA"]]))
    for (w in seq_along(which)) {
      i <- which[[w]]
      if (is.na(i))
        warning(sprintf("Variable %s is missing in DATA", vars[[w]]))
      else 
        x[["data"]][[names(vars)[[w]]]] <- x[["DATA"]][[i]]
    }
    x
  }
}

#' @export
vars_mem_adder <- function(x, regex = NULL, vars = NULL, fn = NULL, ignore.case = FALSE, ...) {
  vars <- match_vars(x, vars, regex, fn, ignore.case, "DATA", fill_names = TRUE)
  function(x, ...) {
    which <- match(vars, names(x[["DATA"]]))
    for (w in seq_along(which)) {
      i <- which[[w]]
      if (is.na(i))
        warning(sprintf("Variable %s is missing in DATA", vars[[w]]))
      else 
        x[["data"]][[names(vars)[[w]]]] <- x[["DATA"]][[i]]
    }
    x
  }
}


#' @export
reclass <- function(x, class, levels) {
  if (!"factor" %in% class && inherits(x, class))
    return(x)
  switch(class[[1]],
         POSIXct = ,
         POSIXlt = lubridate::as_datetime(x),
         Date = lubridate::as_date(x),
         numeric = as.numeric(x),
         integer = as.integer(x),
         logical = as.logical(x),
         character = as.character(x),
         ## FIXME: remove as.character 
         ordered = fct_set_levels(as.character(x), levels),
         factor = fct_set_levels(as.character(x), levels),
         integer64 = bit64::as.integer64(x),
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
    data <- x[["data"]]
    for (nm in names(prototype)) {
      p <- prototype[[nm]]
      v <- data[[nm]]
      if (is.null(v)) {
        if (verbose) {
          message(sprintf("Missing variable '%s' of class '%s'", nm, class(p)))
        }
        data[[nm]] <- p
      } else {
        data[[nm]] <- reclass(v, class(p), levels(p))
      }
    }
    x[["data"]] <- select_vars(data, names(prototype))
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

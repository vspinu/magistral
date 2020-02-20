
#' @export
vars_remover <- function(x = NULL, regex = NULL, vars = NULL, fun = NULL, ignore.case = FALSE, ...) {
  function(x, ...) {
    vars <- sort(c(if (!is.null(regex))
      unlist(map(regex, ~ grep(.x, names(x[["data"]]), ignore.case = ignore.case, value = TRUE))),
      if (!is.null(fun))
        fun(x[["data"]]), 
      vars))
    vars <- intersect(vars, names(x[["data"]]))
    select(setDT(x[["data"]]), -one_of(vars)) %>%
      plug_data(x)
  }
}
 
#' @export
vars_mem_remover <- function(x, regex = NULL, vars = NULL, fun = NULL, ignore.case = FALSE, ...) {
  function(x, ...) {
    vars <- sort(c(if (!is.null(regex))
                     unlist(map(regex, ~ grep(.x, names(x[["DATA"]]), ignore.case = ignore.case, value = TRUE))),
                   if (!is.null(fun))
                     fun(x[["DATA"]]), 
                   vars))
    function(x, ...) {
      vars <- intersect(vars, names(x[["data"]]))
      if (length(vars) > 0) {
        select(x[["data"]], -one_of(vars)) %>%
          plug_data(x)
      } else {
        x
      }
    }
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
    select(setDT(x[["data"]]), vars) %>%
      plug_data(x)
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
  function(x, ...) {
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
    select(out, !!!names(prototype)) %>%
      plug_data(x)
  }
}


#' @export
cat_lumper <- function(x, prop = 0, ...) {
  cat_levels <- list()
  function(x, ...) {
    new_facts <- keep(names(x[["data"]]), function(nm) {
      vec <- x[["data"]][[nm]]
      !nm %in% names(cat_levels) && (is.character(vec) || is.factor(vec))
    })
    new_levels <- map(select(x[["data"]], one_of(new_facts)),
                      fct_lumped_levels, prop = prop)
    cat_levels <<- c(cat_levels, new_levels)
    imodify(x[["data"]], function(v, nm) {
      if ((is.character(v) || is.factor(v)) & nm %in% names(cat_levels)) {
        fct_set_levels(v, cat_levels[[nm]])
      } else {
        v
      }
    }) %>%
      plug_data(x)
  }
}

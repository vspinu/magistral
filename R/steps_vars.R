
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
        select(setDT(x[["data"]]), -one_of(vars)) %>%
          plug_data(x)
      } else {
        x
      }
    }
  }
}

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

vars_adder <- function(x, regex = NULL, vars = NULL, fn = NULL, ignore.case = TRUE, ...) {
  function(x, ...) {
    vars <- sort(c(if (!is.null(regex))
      unlist(map(regex, ~ grep(.x, names(x[["DATA"]]), ignore.case = ignore.case, value = TRUE))),
      if (!is.null(fn))
        fn(x[["data"]]), 
      vars))
    vars <- setdiff(vars, names(x[["data"]]))
    for (var in vars) {
      x[["data"]][[var]] <- x[["DATA"]][[var]]
    }
    x
  }
}

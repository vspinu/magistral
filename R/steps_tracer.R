
tracer <- function(fn) {
  function(x, verbose = TRUE, ...) {
    if (verbose) {
      ovars <- colnames(ml_data(x))
      otime <- Sys.time()
    }
    x <- fn(x)
    if (verbose) {
      tdiff <- Sys.time() - otime
      nvars <- colnames(ml_data(x))
      add_vars <- setdiff(nvars, ovars)
      rem_vars <- setdiff(ovars, nvars)
      added_vars <-
        if (length(add_vars) > 0) sprintf("ADDED: [%s]", paste(add_vars, collapse = ", "))
      else ""
      removed_vars <- 
        if (length(rem_vars) > 0) sprintf("REMOVED: [%s]", paste(rem_vars, collapse = ", "))
      else ""
      catlog(sprintf("%s [%.3f%s] %s %s",
                     paste(x$state$stage, collapse = "->"),
                     as.numeric(tdiff), attr(tdiff, "units"),
                     added_vars, removed_vars))
    }
    x
  }
}

##' @export
wrap_tracer <- function(x, ...) {
  if (x$state$stage[[1]] == "init") {
    x$state$wp$logger <- tracer
    ## remove myself
    x$state$pl[[x$state$ix]] <- NULL
    x$state$ix <- NULL
  }
  x
}
class(wrap_tracer) <- c("magistral.step", "function")

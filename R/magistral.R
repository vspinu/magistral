#' Lightweight machine learning and data processing pipeline
#'
#' @rawNamespace import(rlang, except = c(modify,prepend,`:=`))
#' @importFrom dplyr select one_of
#' @importFrom data.table := rbindlist setkey is.data.table
#' @keywords internal
"_PACKAGE"

#' @export
plug_data <- function(data, x) {
  x[["data"]] <- data
  x
}

.empty_data.table <- function(n) {
  structure(
    list(),
    names = character(),
    class = c("data.frame", "data.table"),
    row.names = .set_row_names(n)
  )
}

normalize_magistral_input <- function(x, ...) {
  if (!is.list(x) || is.data.frame(x)) {
    x <- list(data = x, DATA = x)
  }
  class(x) <- "magistral.state"
  x
}

.ls_fn_env <- function(fn) {
  env <- environment(fn)
  envs <- list()
  while(env_name(env) == "") {
    envs[[length(envs) + 1]] <- env
    env <- parent.env(env)
  }
  envs <- rev(envs)
  cat(sprintf("*%s*", rlang::env_label(env)), "\n", sep = "")
  for (j in seq_along(envs)) {
    pref1 <- paste(rep.int("->", j), collapse = "")
    cat(pref1, " ", sprintf("*%s*", rlang::env_label(envs[[j]])), "\n", sep = "")
    if (length(envs[[j]]) > 0) {
      objects <- as.list.environment(envs[[j]], all.names = TRUE)
      indent <- paste(rep.int("  ", j), collapse = " ")
      str(objects, nest.lev = j, 
          indent.str = indent, no.list = TRUE,
          vec.len = 30)
    }
  }
}

step_fn <- function(step, env) {
  if (is.function(step)) {
    step
  } else if (is_character(step)) {
    get(step, envir = env, mode = "function")
  } else if (is_symbol(step)) {
    get(as.character(step), envir = env, mode = "function")
  } else if (is_formula(step)) {
    rlang::as_function(step, env)
  } else if (is_call(step)) {
    get(as.character(step[[1]]), envir = env, mode = "function")
  } else {
    rlang::abort("Invalid pipeline step. Must be one a function, symbol, character or a formula.", step = step)
  }
}

step_name <- function(step) {
  if (is.function(step) || is_formula(step)) {
    "(anonymous)"
  } else if (is_character(step)) {
    step
  } else if (is_symbol(step)){
    as.character(step)
  } else if (is_call(step)) {
    ## function(...) {...} will result in "function"
    ## xyz(...) will result in "xyz"
    as.character(step[[1]])
  } else stop("Invalid pipeline step object")
}

normalize_pipeline <- function(pl, env = parent.frame()) {
  fn_symbol <- as.symbol("function")
  names <- names(pl)
  for (ix in seq_along(pl)) {
    obj <- pl[[ix]]
    ## set missing name
    if (names[[ix]] == "") {
      names[[ix]] <- step_name(obj)
    }
    if (is_call(obj)) {
      if (identical(obj[[1]], fn_symbol)) {
        ## unquote inline functions
        pl[[ix]] <- eval(obj, envir = env)
      } else {
        ## check for all named arguments of expressions
        if (!all(nzchar(call_args_names(obj)))) {
          stop(sprintf("Empty arguments in step '%s'", names[[ix]]), call. = FALSE)
        }
      }
    }
  }
  names(pl) <- make.unique(names)
  pl
}

## run_hooks <- function(hooks, env) {
##   hook_call <- quote(`_hook`(`_out`))
##   for (hook in hooks) {
##     env[["_hook"]] <<- hook
##     `_out` <<- .Call(purrr:::purrr_eval, hook_call, env)
##   }
##   env[["_hook"]] <- NULL
## }

clean_env <- function(env = NULL, clean_promises = FALSE) {
  ## cleanup promises
  names <- ls(env, all.names = T)
  to_remove <- c("_x_", "_wrapper_")
  if (clean_promises && environmentName(env) == "") {
    to_remove <- c("x", "...", to_remove)
  }
  rm(list = intersect(to_remove, names),
     envir = env)
}

#' @export
stage_runner <- function(x, stage0, call, pl, wrappers, env) {
  ## complete reset of state
  x$state$stage <- stage0
  x$state$ix <- 1
  x$state$pl <- pl
  x$state$wp <- wrappers
  call[[2]] <- quote(`_x_`)
  call0 <- quote(dummy(`_x_`))
  call_tail <- as.list(call)[-(1:2)]
  wrap <-
    if (!is.null(wrappers)) {
      compose(!!!wrappers)
    }
  run1 <- function(x) {
    pl <- x$state$pl
    ix <- x$state$ix
    wp <- x$state$wp
    if (ix > length(pl)) {
      return(NULL)
    }
    cur_stage <- x$state$stage
    step_name <- names(pl)[[ix]]
    stage <- c(cur_stage, step_name)
    stage_id <- paste0(paste(stage, collapse = ":"), "#", step_name(pl[[ix]]))
    x$state$stage <- stage
    obj <- step_fn(pl[[ix]], env)
    if (inherits(obj, "magistral.pipeline")) {
      obj <- environment(obj)[["PIPELINE"]]
    }
    if (is.list(obj)) {
      ## 1. Nested Pipeline 
      # FIXME: wrappers propagate downwards but not upwards? Pick from obj env?
      x <- stage_runner(x, stage, call, obj, wp, env)
      pl[[ix]] <- x$state$pl
      x$state$pl <- pl
      x$state$ix <- ix
      x$state$wp <- wp 
    } else if (stage0[[1]] %in% c("run", "build") || inherits(obj, "magistral.step")) {
      ## 2. Plain Step
      step <- pl[[ix]]
      eenv <- env(env)
      eenv[["_x_"]] <- x
      eenv[[stage_id]] <- obj
      exec <-
        function(x) {
          if (is_call(step)) 
            call <- call_modify(call0, !!!as.list(step)[-1], !!!call_tail, .homonyms = "last")
          call[[1]] <- as.symbol(stage_id)
          while (is.function(obj)) {
            clean_env(environment(eenv[[stage_id]]), TRUE)
            eenv[[stage_id]] <- obj
            obj <- .Call(purrr:::purrr_eval, call, eenv)
          }
          obj
        }
      if (is.null(x$state$wp)) {
        x <- exec(obj)
      } else {
        call[[1]] <- as.symbol("_wrapper_")
        eenv[["_wrapper_"]] <- purrr::compose(!!!x$state$wp)(exec)
        x <- .Call(purrr:::purrr_eval, call, eenv)
      }
      if (is.null(x$state$ix)) {
        # self-deleted
        x$state$ix <- ix
      } else {
        # assign back the realized step for higher order steps
        if (!identical(obj, eenv[[stage_id]])) {
          x$state$pl[[x$state$ix]] <- eenv[[stage_id]]
        }
      }
      ## FIXME: probably needed here for the wrapper
      clean_env(environment(eenv[[stage_id]]), TRUE)
      clean_env(eenv) # needed in case eenv is captured by functions in the call
    }
    x$state$stage <- cur_stage
    x$state$ix <- x$state$ix + 1
    x
  }
  while (x$state$ix <= length(x$state$pl)) {
    x <- run1(x)
  }
  clean_env(env, FALSE)
  x$state$pl <- realized_pl(x$state$pl, x$state$wp)
  x
}

realized_pl <- function(pl, wrappers) {
  structure(pl(.pl = pl, .wrappers = wrappers),
            class = c("magistral.realized.pipeline",
                      "magistral.pipeline",
                      "function"))
}

#' @export
pl <- function(..., .pl = list(), .wrappers = NULL) {

  ENV <- env(caller_env())
  PIPELINE <- normalize_pipeline(
    c(enexprs(..., .named = FALSE, .ignore_empty = "all"), .pl),
    ENV)
  stopifnot(length(PIPELINE) > 0)
  WRAPPERS <- .wrappers

  runner <- function(x, ..., .pl = FALSE) {
    call <- sys.call()
    x <- normalize_magistral_input(x)
    x <- stage_runner(x, "init", call, PIPELINE, WRAPPERS, ENV)
    x <- stage_runner(x, "run", call, environment(x$state$pl)[["PIPELINE"]], x$state$wp, ENV)
    x <- stage_runner(x, "finit", call, environment(x$state$pl)[["PIPELINE"]], x$state$wp, ENV)
    x$pipeline <- x$state$pl
    class(x) <- "magistral.state"
    x[["state"]] <- NULL
    invisible(x)
  }
 
  rm_doted()
  ## formals(runner) <- formals(PIPELINE[[1]])
  ## names(formals(runner))[[1]] <- "x"
  
  structure(runner, class = c("magistral.pipeline", "function"))
}


### Extractors

.ml_datum <- function(x, loc, split, vars) {
  if (is.null(split)) {
    if (is.null(vars)) x[[loc]]
    else select(x[[loc]], one_of(vars))
  } else {
    if (is.character(split)) {
      stopifnot(length(split) == 1)
      splits <- strsplit(split, " *[,:] *")[[1]]
      ix <- unlist(map(splits, ~ x[["splits"]][[.x]]))
    } else {
      ix <- split
    }
    if (is.null(vars)) x[[loc]][ix, ]
    else select(x[[loc]], one_of(vars))[ix, ]
  } 
}

#' @export
ml_data <- function(x, split = NULL, vars = NULL) {
  .ml_datum(x, "data", split, vars)
}

#' @export
ml_DATA <- function(x, split = NULL, vars = NULL) {
  .ml_datum(x, "DATA", split, vars)
}



### Methods

#' @export
`$.magistral.pipeline` <- function(x, name) {
  environment(x)[["PIPELINE"]][[name]]
}

#' @export
.DollarNames.magistral.pipeline <- function(x, pattern = "") {
  pl <- environment(x)[["PIPELINE"]]
  utils:::findMatches(pattern, names(pl))
}

#' @export
print.magistral.pipeline <- function(x, ..., cur_ix = 0) {
  ## pref0 <- paste(if (cur_ix > 0) "\n", rep.int("   ", cur_ix), collapse = "")
  cat(sprintf("<%s>", class(x)[[1]]), "\n", sep = "")
  .ls_fn_env(x)
  pl <- environment(x)[["PIPELINE"]]
  for (i in seq_along(pl)) {
    ix <- if (cur_ix == 0) i else paste(cur_ix, i, sep = ".")
    cat(sprintf("\n%s [%s]: \n", ix, names(pl)[[i]]), sep = "")
    if (is.list(pl[[i]])) {
      for (el in pl[[i]]) {
        print.magistral.pipeline(pl[[i]], ..., cur_ix = ix)
      }
    } else if (inherits(pl[[i]], "magistral.pipeline")) {
      print.magistral.pipeline(pl[[i]], cur_ix = ix)
    } else {
      env <- parent.frame()
      fn <- step_fn(pl[[i]], env)
      if (is.function(fn)) {
        .ls_fn_env(fn)
      } 
      print(pl[[i]], ..., cur_ix = ix)
    }
  }
  invisible(x)
}

#' @export
print.magistral.state <- function(x, ...) {
  str(x)
}

quote_name <- function(name) {
  if (grepl("^[a-zA-Z0-9._]+$", name))
    name
  else paste0("`", name, "`")
}

dput_fn <- function(fn, name, con) {
  name <- quote_name(name)
  env <- environment(fn)
  envs <- list()
  while(env_name(env) == "") {
    envs[[length(envs) + 1]] <- env
    env <- parent.env(env)
  }
  envs <- rev(envs)
  do_local <- any(sapply(envs, length) > 0)
  cat("\n", name, " <- ", if (do_local) "local({" else "", "\n", file = con, sep = "")
  for (env in envs) {
    for (nm in ls(env, all.names = TRUE)) {
      cat("    ", quote_name(nm), " <- ", file = con, sep = "")
      dput(env[[nm]], file = con)
    }
  }
  dput(fn, file = con)
  if (do_local)
    cat("}) # ", name, "\n", file = con, sep = "")
  else
    cat("\n", file = con)
}

#' @export
export.magistral.pipeline <- function(pl, name = NULL, file = "", styler = TRUE,
                                      header = TRUE, extra = character()) {
  if (is.null(name))
    name <- deparse(substitute(pl))
  con <-
    if (!identical(file, "") && is.character(file)) file(file, open = "wt")
    else file
  if(!inherits(con, "connection"))
    stop("'file' must be either a file path or a connection")
  if (header) {
    cat("### --- Auto-generated by magistral v", as.character(packageVersion("magistral")),
        " on ", as.character(Sys.time()), "\n", sep = "", file = con)
    cat("normalize_magistral_input <- \n", file = con)
    dput(normalize_magistral_input, file = con)
    stopifnot(is.character(extra))
    for (nm in extra) {
      where <- 
        if (grepl(":", nm, fixed = T)) {
          nms <- strsplit(nm, ":+")[[1]]
          nm <- nms[[2]]
          asNamespace(nms[[1]])
        } else parent.frame()
      dput_fn(step_fn(nm, where), nm, con)
    }
  }
  pl <- environment(pl)[["PIPELINE"]]
  name <- quote_name(name)
  cat("\n", name, " <- local({\n", sep = "", file = con)
  for (i in seq_along(pl)) {
    nm <- names(pl)[[i]]
    if (inherits(pl[[i]], "magistral.pipeline")) {
      export.magistral.pipeline(pl[[i]], name = nm, file = con, styler = FALSE, header = FALSE)
    } else {
      env <- parent.frame()
      fn <- step_fn(pl[[i]], env)
      dput_fn(fn, nm, con)
    }
  }
  cat("\n  function(x, ...) {\n", file = con, sep = "")
  cat("    x <- normalize_magistral_input(x)\n", file = con)
  for (i in seq_along(pl)) {
    nm <- names(pl)[[i]]
    cat("    x <- ", quote_name(nm), "(x, ...)\n", file = con, sep = "")
  }
  cat("    x\n  }\n}) # ", name, "\n", file = con, sep = "")
  if (is.character(file)) {
    close(con)
    if (styler)
      styler::style_file(file)
  }
  invisible(NULL)
}


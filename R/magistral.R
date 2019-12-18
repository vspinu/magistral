##' Magisterial Package
##' 
##' @import rlang
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

normalize_input <- function(x, ...) {
  if (!is.list(x) || is.data.frame(x)) {
    data <-
      if (is.data.frame(x)) .empty_data.table(nrow(x))
      else if (is.matrix(x)) matrix(nrow = nrow(x), ncol = 0)
      else NULL
    x <- list(data = data, DATA = x)
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
  cat(glue("*{rlang::env_label(env)}*"), "\n", sep = "")
  for (j in seq_along(envs)) {
    pref1 <- paste(rep.int("->", j), collapse = "")
    cat(pref1, " ", glue("*{rlang::env_label(envs[[j]])}*"), "\n", sep = "")
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
          stop(glue("Empty arguments in step '{names[[ix]]}'"), call. = FALSE)
        }
      }
    }
  }
  names(pl) <- make.unique(names)
  pl
}

run_hooks <- function(hooks, env) {
  hook_call <- quote(`_hook`(`_out`))
  for (hook in hooks) {
    env[["_hook"]] <<- hook
    `_out` <<- .Call(purrr:::purrr_eval, hook_call, env)
  }
  env[["_hook"]] <- NULL
}

clean_env <- function(env = NULL, clean_promises = FALSE) {
  ## cleanup promises
  names <- ls(fnenv, all.names = T)
  to_remove <- c("_x_", "_wrapper_")
  if (clean_promises && environmentName(fnenv) == "") {
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
      clean_env(environment(eenv[[stage_id]]), TRUE)
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
    x <- normalize_input(x)
    x <- stage_runner(x, "init", call, PIPELINE, WRAPPERS, ENV)
    x <- stage_runner(x, "run", call, x$state$pl$PIPELINE, x$state$wp, ENV)
    x <- stage_runner(x, "finit", call, x$state$pl$PIPELINE, x$state$wp, ENV)
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
  cat(glue("<{class(x)[[1]]}>"), "\n", sep = "")
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
      if (is.function(pl[[i]])) {
        .ls_fn_env(pl[[i]])
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

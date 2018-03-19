##' @import rlang
##' @importFrom purrr map map_chr map_lgl map_int map_dbl 
##' @importFrom fastdigest fastdigest


## utils

remove_dots <- function(args){
    if ("..." %in% names(args)) # https://github.com/tidyverse/rlang/issues/393
        args[["..."]] <- NULL
    args
}

## drops all unnamed args
merge_lists <- function(...) {
    arg_list <- map(ll(...), as.list)
    reduce(arg_list, modifyList)
}

ml_return <- function(x, ret = x$ret, ...) {
    update_in(x, "ret", ret, ...)
}

`_mlhints` <- new.env()
mlhint <- function(obj) {
    type <- attr(obj, "mlhint")
    if (is.null(type)) {
        type <- "NULL"
        if (is.call(obj)) {
            type <- `_mlhints`[[as.character(obj[[1]])]]
            if (is.null(type))
                type <- "NULL"
        }
    }
    type
}


## DISPATCH

invoke_stack_item <- function(item, x, args = NULL, env) {
    if (is_call(item)) {
        no_merge <- is.null(args) || call_name(item) %in% "mlfn" 
        args <-
            if (no_merge) c(list(x), call_args(item))
            else merge_lists(list(x), call_args(item), args[-1])
        next_call <- new_call(sym(call_name(item)), as.pairlist(args))
        eval_bare(next_call, env = env)
    } else if (is_function(item)) {
        args[[1]] <- x
        do.call(item, args, envir = env)
    } else {
        stop(sprintf("invalid item type (%s)", typeof(item)))
    }
}

##' @export
mlcontinue <- function(x, op = x[["op"]]) {
    ._ml_marker_. <- "mlcontinue"
    x[["op"]] <- op
    cxtenv <- x[["cxtenv"]]
    if (or(cxtenv[["inhibit_continuation"]])) {
        return(x)
    }
    branch <- x[["full-branch"]]
    if (is.null(pluck(x, "op-markers", op, branch))) {
        x <- assoc(x, c("op-markers", op, branch), FALSE)
    }
    if (is.null(cxtenv))
        stop("'cxtenv' is missing")
    stack <- cxtenv[["stack"]]
    ## update cxtenv or return
    pos <- cxtenv[["pos"]] + 1L
    if (pos == length(stack) + 1L)
        return(x)
    x[["cxtenv"]][["pos"]] <- pos
    ## store matched args_so_far
    curitem <- stack[[pos]]
    curname <- names(stack)[[pos]]
    curtime <- Sys.time()
    curid <- c(format(as.numeric(curtime), nsmall = 6), op, x[["full-branch"]], pos, curname)
    curhash <- fastdigest(curid)
    x[["cxtenv"]][["id"]] <- curid
    x[["cxtenv"]][["idhash"]] <- curhash
    x[["cxtenv"]][["time"]] <- curtime
    x <- update_in(x, list("cxtenv", "path"),
                   append, ll(!!curhash := curid))
    xnew <-
        if (is.mlstack(curitem)) {
            tx <- assoc(x, c("cxtenv", "stack"), curitem)
            tx <- mldispatch(tx, op, branch = curname, start_pos = 1)
            tx <- assoc(tx, c("cxtenv", "stack"), x[["cxtenv"]][["stack"]])
            assoc(tx, "full-branch", branch)
        } else {
            curargs <- item_args(curitem)
            all_args <- cxtenv[["all_args"]]
            if (!"skip_args_so_far" %in% mlhint(curitem))
                x[["cxtenv"]][["args_so_far"]] <-
                    modifyList(as.list(cxtenv[["args_so_far"]]),
                               all_args[names(all_args) %in% names(curargs)])
            invoke_stack_item(curitem, x, cxtenv[["call_args"]], cxtenv[["env"]])
        }
    assoc(xnew, c("cxtenv", "pos"), pos)
}

##' @export
mldispatch <- function(x, op = x[["op"]], branch = NULL, start_pos = 1L) {
    ._ml_marker_. <- "mldispatch"
    old_op <- x[["op"]]
    old_branch <- x[["branch"]]
    old_full_branch <- x[["full-branch"]]
    branch <- or(branch, model_name(x))
    full_branch <- paste(old_full_branch, branch, sep = ":")
    x <- assoc(x,
               "op", op,
               "branch", branch,
               "full-branch", full_branch,
               c("cxtenv", "pos"), start_pos - 1L, 
               c("start_time", op), Sys.time(), 
               c("op-markers", op, full_branch), FALSE)
    x <- mlcontinue(x)
    assoc(x,
          "op", old_op,
          "branch", old_branch,
          "full-branch", old_full_branch)
}

dispatcher_fn <- function(name = NULL, args, parent_env, stack, call_args = list()) {
    env <- child_env(parent_env, MLSTACK.. = stack, MLARGS.. = call_args)
    xname <- names(args)[[1]]
    if (is.null(xname))
        stop("first arg must be named")
    args <- normalize_args(args)
    args[[1L]] <- quote(identity)
    new_function(
        args = args,
        body = expr(
            ## ARGS are picked by mlfunction and init_context
            if (is.function(!!sym(xname))) {
                mlfunction(!!name)
            } else {
                x <- ml_ingest(!!sym(xname))
                x <- ml_init_context(x)
                x <- mldispatch(x, "init")
                x <- mldispatch(x, "run")
                mldispatch(x, "finish")
            }), 
        env = env)
}

##' @export
mlfunction <- function(name = NULL,
                       env = caller_env(2), 
                       call = caller_call(),
                       fn = caller_fn()) {
    call_args <- call_args(match.call(fn, call, envir = env))
    my_args <- modifyList(fn_fmls(fn), call_args)
    up_fn <- eval_bare(my_args[[1]], env)
    stopifnot(is.function(up_fn))
    upstack <- environment(up_fn)$MLSTACK.. # NULL if up_fn is not mlfunction
    ## remove first arg when explicitly supplied (pipes & composition)
    if (length(call_args) > 0 && names(my_args)[[1]] == names(call_args)[[1]])
        call_args <- call_args[-1]
    mystack <-
        if (is.null(name)) {
            ## auto generated mlfunction
            add_args_to_stack(env_get(fn, "MLSTACK.."), call_args)
        } else {
            ## standard mlfunctions
            ll(!!name := new_call(sym(name), as.pairlist(call_args)))
        }   
    all_args <- merge_lists(my_args, fn_fmls(up_fn), call_args)
    dispatcher_fn(name = name,
                  args = all_args,
                  parent_env = env,
                  stack = c(upstack, mystack),
                  call_args = call_args)
}

##' Internal functions
##'
##' These functions are exported due to the nature of the `multiline` dispatch.
##'
##' @param x ml context
##' @param env env containing MLSTACK..
##' @param stack MLSTACK..
##' @param call caller call
##' @param fn caller fn
##' @rdname internal
##' @keywords internal
##' @export
ml_init_context <- function(x, env = env_parent(caller_env()),
                            stack = env_get(env, "MLSTACK.."),
                            call = caller_call(),
                            fn = caller_fn()) {
    fn_args <- remove_dots(fn_fmls(fn))
    call_args <- call_args(match.call(fn, call, envir = env))

    ## seed
    seed <- or(call_args[["random_seed"]], fn_args[["random_seed"]], 0)
    if (is.list(seed)) do.call(set.seed, seed)
    else set.seed(seed)
    x[["seed"]] <- seed

    ## cxtenv
    args <- modifyList(fn_args, call_args)
    cxt <- ll(env = env,
              all_args = as.list(args),
              call_args = call_args, 
              stack = stack)
    x[["cxtenv"]] <- cxt
    
    x
}

normalize_args <- function(args) {
    ## todo: move all .args after ...
    if ("..." %in% names(args))
        args[["..."]] <- NULL
    args[["..."]] <- sym("")
    args
}



## user level

##' @export
nocont <- function(f) {
    function(...) {
        args <- list(...)
        old_ic <- args[[1]][["cxtenv"]][["inhibit_continuation"]]
        args[[1]][["cxtenv"]][["inhibit_continuation"]] <- TRUE
        x <- do.call(f, args)
        x[["cxtenv"]][["inhibit_continuation"]] <- old_ic
        x
    }
}

##' @export
mlfn <- function(x = identity, ..., .env = caller_env(), .name = "anonymous") {
    dots <- exprs(...)
    len <- length(dots)
    args <- dots[1:(len - 1)]
    nonames <- !nzchar(names(args))
    names(args)[nonames] <- as.character(args[nonames])
    args[nonames] <- list(sym(""))
    args[[1]] <- quote(identity)
    names(args)[[1]] <- "x"
    args[["..."]] <- sym("")
    body <- dots[[len]]
    if (nzchar(names(dots)[[len]]))
        stop("Body (last element of `...`) must not be named")
    
    fn <-
        set_attrs(new_function(args = args,
                               body = expr(
                                   if (is.function(x)) {
                                       mlfunction(NULL)
                                   } else {
                                       !!!body
                                   }),
                               env = child_env(.env)),
                  mlfn = TRUE)
    environment(fn)[["MLSTACK.."]] <- ll(!!.name := fn)
    environment(fn)[["MLARGS.."]] <- list()
    
    ## xname <- names(args)[[1]]
    do.call(fn, ll(x = x), envir = .env) 
}

##' @export
ml <- function(x = identity, ..., .name = "ml") {
    ## args <-
    ##     if (!missing(x)) {
    ##         stopifnot(is.function(x))
    ##         fn_fmls(x)
    ##     }
    cenv <- caller_env()
    raw_stack <- exprs(..., .ignore_empty = "all")
    raw_stack <- map(exprs(..., .ignore_empty = "all"),
                     ~ new_call(node_car(.x), node(sym("."), .x[-1])))
    fn <- reduce(raw_stack, ~ invoke_stack_item(.y, x = .x, args = NULL, cenv), .init = identity)
    mystack <- set_attrs(environment(fn)$MLSTACK.., class = "mlstack")
    upstack <- environment(x)$MLSTACK..
    stack <- append(upstack, ll(!!.name := mystack))
    dispatcher_fn(args = fn_fmls(fn),
                  parent_env = cenv, 
                  stack = stack)
}

##' @export
mlrepeat <- function(x = identity, nrepeats = 10, ...) {
    if (is.function(x))
        return(mlfunction("mlrepeat"))
    xout <- mllist()
    old_repeat <- x[["repeat"]]
    for (i in seq_len(nrepeats)) {
        branch <- paste0("repeat", i)
        x[["repeat"]] <- i
        tx <- mldispatch(x, branch = branch, start_pos = x[["cxtenv"]][["pos"]] + 1L)
        xout <- modifyList(xout, tx)
    }
    assoc(xout, "repeat", old_repeat)
}


### MLLIST

##' @export
mllist <- function(x = NULL) {
    set_attrs(as.list(x), class = c("mllist", "list"))
}

is.mllist <- function(x) {
    is.list(x) && "mllist" %in% class(x)
}

##' @export
print.mllist <- function(x, ...) {
    str(unclass(x), vec.len = 50, width = 300, no.list = TRUE)
}

##' @rdname internal
##' @keywords internal
##' @export
ml_ingest <- function(x = identity, ...) {
    if (is.function(x)) {
        mlfunction("ml_ingest")
    } else if (is.mllist(x)) {
        x
    } else {
        mllist(list(data = x))    
    }
}

check_data_nrow <- function(x) {
    data <- x[["data"]]
    if (is.data.frame(data)) {
        nrow(x)
    } else if (is.list(data)) {
        nrows <- sort(unique(map_int(data, NROW)))
        if (length(nrows) > 1) {
            if (nrows[[1]] != 1)
                stop(sprintf("Invalid minimal data size (%d); 1 expected", nrows[[1]]))
            if (length(nrows) > 2)
                stop(sprintf("Invalid data sizes (%s)", paste(nrows, collapse = ", ")))
        }
        max(nrows)
    } else {
        stop("'data' component must be a data.frame or a list")
    }
}

data_nrow <- function(x) {
    check_data_nrow(x)
}

data_subset <- function(x, subset) {
    data <- x[["data"]]
    if (is.data.frame(x)) {
        assoc(x, "data", data[subset, ])
    } else if (is.list(data)) {
        data <-
            map(data, function(e) {
                if (length(e) == 1) e
                else if (!is.null(dim(c())))
                    e[subset, ]
                else
                    e[subset]   
            })
        assoc(x, "data", data)
    } else {
        stop("'data' component must be a data.frame or a list")        
    }
}



### STACK

##' @export
mlstack <- function(x) {
    if (is.function(x)) {
        mlstack(environment(x)[["MLSTACK.."]])
    } else {
        class(x) <- "mlstack"
        x
    }
}

##' @export
print.mlstack <- function(x, ...) {
    str(unclass(x), vec.len = 50, width = 300, no.list = TRUE)
}

is.mlstack <- function(x) {
    identical(class(x), "mlstack")
}


or <- function(x, y = NULL, z = NULL) {
    if (is.null(x) || (is.logical(x) && !x))
        if (is.null(y) || (is.logical(y) && !y))
            if (is.null(z)) FALSE
            else z
        else y
    else x
}

## update.list <- function(object, ...) {
##     dots <- dots_list(...)
##     purrr:::list_recurse(object, dots, function(x, y) y)
## }
## registerS3method("update", "list",  "update.list")

## ## deep_get <- function(object, keys) {
## ##     for (k in keys) {
## ##         obj <- object[[k]]
## ##         if (is.null(obj))
## ##             return(NULL)
## ##     }
## ##     obj
## ## }

## ##' @export
## assoc <- function(object, key, val) {
##     object[[key]] <- val
##     object
## }

## x[["a"]][["b"]] doesn't create nested on non-existent objects
##' @export
assoc <- function(object, keys = NULL, val = NULL, ...) {
    if (is.null(keys))
        return(object)
    k <- keys[[1]]
    if (length(keys) > 1) {
        obj <- object[[k]]
        if (is.null(obj))
            obj <- list()
        object[[k]] <- assoc(obj, keys[-1], val)
    } else {
        object[[k]] <- val
    }
    assoc(object, ...)
}

##' @export
update_in <- function(object, keys, .f, ...) {
    k <- keys[[1]]
    if (length(keys) > 1) {
        obj <- object[[k]]
        if (is.null(obj))
            obj <- list()
        object[[k]] <- update_in(obj, keys[-1], .f, ...)
    } else {
        object[[k]] <- purrr::partial(.f, ..., .lazy = F, .first = F)(object[[k]])
    }
    object
}

format_difftime <- function(td) {
    x <- as.numeric(td, units = "secs")
    s <- x %% 60; x <- x - s
    m <- (x %% 3600)/60; x <- x - m*60
    h <- x / 3600
    if (h > 0) sprintf("%.0fh%02.0fm%02.0fs", h, m, s)
    else if (m > 0) sprintf("%.0fm%02.0fs", m, s)
    else sprintf("%.2fs", s)    
}

caller_call <- function(n = 1) {
    sys.call(sys.parent(n + 1))
}

get_args <- function(x) {
    if (is_function(x))
        fn_fmls(x)
    else if (is_call(x)) {
        call_args(x)
    }
}

env_parents <- function(env = caller_env(), last = global_env()) {
    env <- get_env(env)
    out <- list()

    do <- TRUE
    i <- 1L 
    while (do) {
        nm <- environmentName(env)
        if (is.null(nm))
            nm <- i
        out[[nm]] <- env
        i <- i + 1L
        do <- !(is_reference(env, last) || rlang:::is_empty_env(env))
        env <- env_parent(env)
    }

    out
}

env_parent_names <- function(env = caller_env()) {
    env_names(env_parent(env))
}

env_parents_names <- function(env = caller_env(), last = global_env()) {
    sapply(env_parents(env, last), env_names, simplify = FALSE)
}

add_args_to_stack <- function(stack, args) {
    for (i in seq_along(stack)) {
        item <- stack[[i]]
        if (is.mlstack(item)) {
            stack[[i]] <- add_args_to_stack(item, args)
        } else if (is.function(item)) {
            fn_fmls(item) <- modifyList(fn_fmls(item), args)
            stack[[i]] <- item
        } else {
            stopifnot(is_call(item))
            new_args <- as.pairlist(modifyList(item_args(item), args))
            stack[[i]] <- new_call(sym(call_name(item)), new_args)
        }
    }
    stack
}

item_args <- function(item) {
    out <-
        if (is.call(item)) call_args(item)
        else if (is.function(item)) fn_fmls(item)[-1]
        else stop("Invalid item")
}


## mlstack <- function(fn, unfold = TRUE) {
##     stack <- get_env(fn)$MLSTACK..
##     if (unfold) {
##         map(stack, function(f) {
##             if (is.function(f)) {
##                 istack <- get_env(fn)$MLSTACK..
##                 if (!is.null(istack) && !is.null(attr(f, "mlstack"))) {
##                     mlstack(f)
##                 } else {
##                     f
##                 }
##             } else {
##                 f
##             }
##         })
##     } else {
##         stack
##     }
## }

fn <- function(..., .env = caller_env()) {
    dots <- exprs(...)
    len <- length(dots)
    args <- dots[1:(len - 1)]

    ## make sure unnamed args appear as names
    nonames <- !nzchar(names2(args))
    names(args)[nonames] <- as.character(args[nonames])
    args[nonames] <- list(sym(""))

    if (nzchar(names(dots)[[len]]))
        stop("Body (last element of `...`) must not be named")

    body <- dots[[len]]
    new_function(args, body, .env)
}

stopif <- function(...) {
    n <- length(ll <- list(...))
    if (n == 0L) 
        return(invisible())
    mc <- match.call()
    for (i in 1L:n)
        if (!is.logical(r <- ll[[i]]) || anyNA(r) || any(r)) {
            ch <- deparse(mc[[i + 1]], width.cutoff = 60L)
            if (length(ch) > 1L)
                ch <- paste(ch[1L], "....")
            stop(sprintf("%s is TRUE", ch), call. = FALSE, domain = NA)
        }
    invisible()
}


## log/cache utilities

output_dir <- function(x, output_dir, name) {
    dir <- paste0(output_dir, "/", model_name(x), "/", name)
    if (!dir.exists(dir))
        dir.create(dir, showWarnings = F, recursive = TRUE)
    dir
    
}

invoke_fns <- function(x, obj, fns, fn_type, ...) {
    for (f in fns) {
        fn <-
            if (is.function(f)) f
            else match.fun(paste0(fn_type, f), descend = F)
        x <- do.call(fn, list(x = x, obj = obj, ...))
    }
    x
}



## context extractors

model_name <- function(x) {
    pluck(x, "model", "name", .default = "unknown")
}

pos <- function(x) {
    x[["cxtenv"]][["pos"]]
}

cur_item_name <- function(x) {
    pos <- pos(x) + 1L
    nms <- names(x[["cxtenv"]][["stack"]])
    if (length(nms) >= pos) nms[[pos]]
    else ""
}

prev_stack_name <- function(x) {
    pos <- pos(x)
    if (pos > 0) names(x[["cxtenv"]][["stack"]])[[pos]]
    else ""
}

##' @export
mlcalls <- function(add_x = FALSE) {
    envs <- rlang:::ctxt_stack_envs()
    wmls <- map_lgl(envs, ~ exists("._ml_marker_.", envir = .x, inherits = FALSE))
    envs <- envs[wmls]
    out <-
        map(envs, function(e) {
            cxt <- e[["x"]][["cxtenv"]]
            pos <- cxt[["pos"]] + 1L
            o <- list(type = e[["._ml_marker_."]], 
                      name =  cur_item_name(e[["x"]]),
                      pos = cxt[["pos"]] + 1L,
                      path = cxt[["path"]], 
                      args = cxt[["call_args"]])
            if (add_x)
                o[["x"]] <- e[["x"]]
            o
        })    
    out <- set_names(out, map_chr(out, "name"))
    structure(out, class = "mlcalls")
}

##' @export
print.mlcalls <- function(x, ...) {
    str(unclass(x), vec.len = 50, width = 300)
}

## fn(x, x + 1)

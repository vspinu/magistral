

### UTILITIES

attr2cache <- function(x) {
    attrs <- attributes(x)
    attrs[!names(attrs) %in% c("row.names", ".internal.selfref")]
}

mldigest <- function(x) {
    digest <- as.list(x[["digest"]])
    asf <- x[["cxtenv"]][["args_so_far"]]
    asf <- asf[sort(names(asf))]
    digest[["args_so_far"]] <- fastdigest(asf)
    digest[["data"]] <- map(x[["data"]], fastdigest)
    digest
}

add_digest <- function(x) {
    x[["digest"]] <- mldigest(x)
    x
}

## todo: a package for incremental caching/diff of recursive R objects?
x2cache <- function(x, old_digest) {
    data <- x[["data"]]
    old_dhash <- old_digest[["data"]]
    new_dhash <- x[["digest"]][["data"]]
    old_names <- intersect(names(data), names(old_dhash))
    x[["data"]] <-
        if (length(old_names)) {
            add_names <- setdiff(names(data), names(old_dhash))
            tosave <- c(keep(old_names, ~ old_dhash[[.x]] != new_dhash[[.x]]), 
                        add_names)
            data[tosave]
        } else {
            data
        }
    x[["data_attributes"]] <- attr2cache(data)
    x[["cxtenv"]] <- NULL
    x
}

cache2x <- function(x, cache) {
    data <- x[["data"]]
    cached_data <- cache[["data"]]
    cached_attr <- cache[["data_attributes"]]
    cache[c("data", "data_attributes")] <- NULL
    x <- modifyList(x, cache)
    for (nm in names(cached_data))
        data[[nm]] <- cached_data[[nm]]
    attributes(data) <- cached_attr
    x[["data"]] <- data
    x
}

cache_file <- function(x, output_dir, prefix, ext) {
    ## fixme: digest should be cached across invocations
    x <- add_digest(x)
    hash <- pluck(x, "digest", "args_so_far")
    stopif(is.null(hash))
    paste0(output_dir(x, output_dir, "cache"),
           "/", model_name(x), ":", hash, ".", ext)
}

marks2list <- function(markers) {
    ## MARKERS is a named list of objects. Each name has the aa:bb:dd. Return a
    ## nested list with nested elements of the form ll$aa$bb$dd.
    out <- list()
    nms <- names(markers)
    for (nm in nms) {
        out <- assoc_in(out, strsplit(nm, ":", fixed = T), markers[[nm]])
    }
    out
}


### DEFAULT CACHERS and LOADERS

cacher_rds <- function(x, obj = NULL, output_dir, compress = FALSE, ..., .action = "load-cache") {
    file <- cache_file(x, output_dir, x[["full-branch"]], "rds")
    switch(.action,
           `cached?` = {
               stopifnot(is.null(obj))
               if (file.exists(file)) {
                   x <- assoc_in(x, c("op-markers", x[["op"]], x[["full-branch"]]), TRUE)
               }
           }, 
           `load-cache` = {
               stopifnot(is.null(obj))
               if (file.exists(file)) {
                   x <- cache2x(x, readRDS(file))
               } else {
                   stop(sprintf("Invalid cache: no such file %s", file))
               }
           },
           `cache` = {
               saveRDS(obj, file = file, compress = compress)
           },
           stop("invalid .action ", .action))
    x
}

cache_continuator <- function(x, ...) {
    if (x$op == "run") {
        xout <- mlcontinue(x, "cached?")
        markers <- unlist(marks2list(xout[["op-markers"]][["cached?"]]))
        has_cache <- all(map_lgl(markers, ~ as.logical(.x)))
        if (has_cache) mlcontinue(x, op = "load-cache")
        else mlcontinue(x)
    } else {
        mlcontinue(x)
    }
}           

ml_cache <- function(x = identity,
                     cacher = "rds",
                     output_dir = "../output/",
                     do_cache = TRUE, ...) {
    if (is.function(x))
        return(mlfunction("ml_cache"))
    fn <-
        if (is.function(cacher)) cacher
        else match.fun(paste0("cacher_", cacher), F)
    if (x[["op"]] == "cached?") {
        return(do.call(fn, list(x, obj = NULL, output_dir = output_dir, ..., .action = "cached?")))
    }
    switch(x$op,
           describe = {
               x$describe[["ml_cache"]] <-
                   ll(doc = "Generic Cacher",
                      handles = c("describe", "init", "run", "cached?"))
               mlcontinue(x)
           },
           init = {
               if (do_cache) {
                   if (is.null(x[["cache"]][["initialized"]])) {
                       x[["cache"]][["initialized"]] <- TRUE
                       x[["cxtenv"]][["stack"]] <- c(ll(cache_continuator = cache_continuator),
                                                     x[["cxtenv"]][["stack"]])
                       x[["cxtenv"]][["pos"]] <- x[["cxtenv"]][["pos"]] + 1L
                   }
                   x <- nocont(ml_log)(x) # initialize the logger
               }
               mlcontinue(x)
           },
           `load-cache` = {
               if (do_cache) {
                   x <- do.call(fn, list(x, NULL, output_dir = output_dir, ..., .action = "load-cache"))
                   nocont(ml_log)(x, msg = sprintf("Loaded cache (pos:%d after:%s)", pos(x), prev_stack_name(x)))
                   cache_continuator(x)
               } else {
                   mlcontinue(x)
               }
           },
           run = {
               nocont(ml_log)(x, sprintf("Caching (pos:%d after:%s)", pos(x), prev_stack_name(x)))
               old_digest <- x[["digest"]]
               obj <- x2cache(add_digest(x), old_digest)
               mlcontinue(do.call(fn, list(x, obj, output_dir = output_dir, ..., .action = "cache")))
           }, 
           mlcontinue(x))
}

`_mlhints`[["ml_cache"]] <- "skip_args_so_far"



## UTILITIES
log_obj <- function(x, msg = list(), ...) {
    if (is.character(msg))
        msg <- list(msg = msg)
    cxtenv <- x[["cxtenv"]]
    cur_time <- Sys.time()
    modifyList(ll(time_total = cur_time - x[["start_time"]][["init"]],
                  time_diff = cur_time - x[["log"]][["prev_time"]], 
                  args_so_far = cxtenv[["args_so_far"]], 
                  pos = pos(x), 
                  after = prev_stack_name(x)),
               msg)
}

log_string <- function(x, obj) {
    msg <- obj[["msg"]]
    if (is.null(msg)) {
        str <- capture.output(str(obj[["args_so_far"]],
                                  no.list = TRUE, give.attr = F, give.head = F))
        params <- paste0(sub(" +:", ":", sub("( +\\$)", "", str)),
                         collapse = "")
        msg <- sprintf("Params:%s", params)
    }
    sprintf("[%s/%s] %s\n",
            format_difftime(obj$time_total),
            format_difftime(obj$time_diff),
            msg)
}


## LOGGERS

logger_txt <- function(x, obj, output_dir, ...) {
    file <- paste0(output_dir(x, output_dir, "log"), "/current.txt")
    msg <- log_string(x, obj)
    cat(msg, file = file, append = TRUE)
    invisible(x)
}

logger_stdout <- function(x, obj, ...) {
    cat(log_string(x, obj))
    invisible(x)
}
 
logger_R <- function(x, obj, output_dir, ...) {
    log_file <- paste0(output_dir(x, output_dir, "log"), "/current.R")
    con <- textConnection("val", "w", local = TRUE)
    on.exit(close(con))
    dput(obj, file = con)
    file <- file(log_file, "a", encoding = "UTF-8")
    on.exit(close(file))
    writeLines(val, file)
    invisible(x)
}


### ML FUNCTION

invoke_loggers <- function(x, msg, loggers, output_dir, ...) {
    for (l in loggers) {
        fn <-
            if (is.function(l)) l
            else match.fun(paste0("logger_", l), descend = F)
        x <- do.call(fn, list(x = x, obj = obj, output_dir = output_dir, ...))
    }
    assoc_in(x, c("log", "prev_time"), Sys.time())
}

ml_log <- function(x = identity, msg = list(),
                   loggers = c("stdout", "R"),
                   output_dir = "../output/", ...) {
    if (is.function(x))
        return(mlfunction("ml_log"))
    mlcontinue(
        switch(x$op,
               describe = {
                   x$describe[["ml_log"]] <-
                       ll(doc = "Generic Logger",
                          handles = c("describe", "init", "run", "log"))
                   x
               },
               init = {
                   if (is.null(x[["log"]][["initialized"]])) {
                       start_time <- x[["start_time"]][["init"]]
                       x <- assoc_in(x, c("log", "prev_time"), start_time)
                       msg <- sprintf("Started '%s' at %s", model_name(x), start_time)
                       x <- invoke_fns(x, log_obj(x, msg), loggers, "logger_",
                                       output_dir = output_dir, ...)
                       x[["log"]][["initialized"]] <- TRUE
                   }
                   x
               }, 
               run = {
                   invoke_fns(x, log_obj(x, msg), loggers, "logger_",
                              output_dir = output_dir, ...) 
               }, 
               x))
}

`_mlhints`[["ml_log"]] <- "skip_args_so_far"


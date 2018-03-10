
model(model = "my-model", seed = 123,
      data_dir = "../input/",  out_dir = "../out/", cache_dir = "../cache/",

      checkpoint_load(checkpoint_dir = paste0(cache_dir, "/checkpoints")), 
      backup_zip(backup_dir = paste0(cache_dir, "./backups")), 
      data(data = NULL,
           
           split_train(), 
           shufle(), 
           add_unknowns(unknown_prob = .01), 
           add_silence(silence_prob = .01), 
           blend_noize(), 
           time_shift(), 
           wave_to_melspetogram(), 
           batch(batch_size = 256, drop_last = True)),
      
      log(), 
      iterate(lr = 0.01/iter_accum,
              momentum = 0.9, decay = 0.0001, 
              iter_start = 0, iter_end = 100,
              iter_smooth = 20, iter_checkpoint = 500,
              lr = 0.01, lr_start = lr[1], lr_end = NULL,
              
              sgd(lr = 0.01/iter_accum, iter_accum = 1), 
              on_iter(iter_valid = 20, 
                      eval(), 
                      log()), 
              on_iter(iter_checkpoint = 500, 
                      checkpoint_save()), 
              adjust_lr(), 
              compute_batch_loss(), 
              print_loss()))


iterate.default <- function(x, ...) {
    x
}


get_args <- function(item, fn = NULL, env = caller_env(), all = F) {
    if (is.function(item)) {
        fn_fmls(item)
    } else if (is_call(item)) {
        if (is.null(call_name(item))) {
            ## result of do.call 
            fn <- item[[1]]
            if (is.function(fn)) {
                call_args(item)
            } else {
                stop("invalid item[[1]]")
            }
        } else {
            if (is.null(fn) && !is.null(call_name(item)))
                fn <- get(call_name(item), mode = "function")
            call_args <- call_args(match.call(fn, item, envir = env))
            if (all) {
                sym0 <- sym("")
                fmls <- discard(fn_fmls(fn), is_missing)
                as.pairlist(modifyList(fmls, as.list(call_args)))
            } else 
                call_args
        }
    } else {
        stop("Invalid type of `item` (%s); must be either `call` or `function`")
    }
}



library(pryr)
a <- 1:50
b <- 1:50
c(address(a), refs(a))
tt <- list(a = a, b = b)
a1 <- tt[["a"]]
c(address(a1), refs(a1))
tt[["c"]] <- 123
a1 <- tt[["a"]]
c(address(a1), refs(a1))
tt1 <- tt
tt1[["d"]] <- 34
a2 <- tt1[["a"]]
c(address(a2), refs(a2))
tfn <- function(x) {
    x[["p"]] <- 343
    x
}
tt2 <- tfn(tt)
a4 <- tt2[["a"]]
c(address(a4), refs(a4))

tn <- list(tt = tt)
tn[["tt"]][["k"]] <- 343
a5 <- tn[["tt"]][["a"]]
c(address(a5), refs(a5))

tt6 <- modifyList(tt, list(b = "34"))
a6 <- tt6[["a"]]
c(address(a6), refs(a6))
attributes(a6) <- c(attributes(a6), ll(bb = 343))
a6 <- tt6[["a"]]
c(address(a6), refs(a6))
tt7 <- as.list(tt6)
a7 <- tt7[["a"]]
c(address(a7), refs(a7))

Sys.timezone()

Sys.unsetenv("TZ")
## Sys.setenv(TZ = "America/New_York")
## Sys.getenv("TZ")
lubridate:::C_local_tz()
as.POSIXct("2017-08-13", tz = "")
ymd("2017-08-13", tz = "")
force_tz(as.POSIXct("2017-08-13", tz = ""), "UTC")

t1 <- as.POSIXct("2018-02-28 20:10:00")
lubridate::minute(t1) <- 0
t1
as.POSIXct("2017-07-01", format = "%Y-%m-%d") + days(1)

tt <- list(a = 1:4, b = 1:3)
a <- tt[["a"]]
address(a)
tt2 <- append(tt, list(c = 1:3))
a2 <- tt2[["a"]]
address(a2)

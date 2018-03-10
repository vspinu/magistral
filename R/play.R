
source("./utils.R")
source("./pipeline.R")
source("./log.R")
source("./cache.R")

a <- function(x = identity, arg_a = 0, ...) {
    if (is.function(x))
        return(mlfunction("a"))
    mlcontinue(
        switch(x$op,
               describe = {
                   descr <- ll(doc = "Brief Documentation",
                               handles = c("run", "describe", "a"))
                   modifyList(x, describe = c(x$describe, a = descr))
               },
               run = {
                   modifyList(x, ll(a_arg_a = arg_a, a_dots = ll(...)))   
               },
               x))
}

b <- function(x = identity, arg_b = 0, arg_b_default = 0, ...) {
    if (is.function(x))
        return(mlfunction("b"))
    mlcontinue(
        switch(x$op,
               describe = {
                   descr <- ll(doc = "B Doc",
                               handles = c("run", "describe", "b"))
                   modifyList(x, describe = c(x$describe, b = descr))
               },
               run = {
                   modifyList(x, ll(b_arg_b = arg_b, b_dots = ll(...)))   
               },
               x))
}

x <- 12
tta <-
    a(arg_a = 1, other = 1) %>%
    ml_log("Some log message") %>% 
    b(arg_b = 101, other = 100) %>%
    ml_log() %>%
    ## ml_cache() %>% 
    mlfn(mlcontinue(modifyList(x, ll(mlfn_a = "mlfn_a"))))

tta(iris, arg_a = 900, extra_arg = 900)

ttb <-
    ml(identity,
       ## a(arg_a = 2, other = 2),
       b(arg_b = 200, other = 200),
       mlfn(mlfn_arg = 3, {
           mlcontinue(modifyList(x, ll(mlfn_b_arg = mlfn_arg)))
       }))

ttb(2 + x, arg_b = 900, extra_arg = 900)

ttc <- tta() %>% ttb()
ttc(2 + x)
ttc(2 + x, arg_a = "ttb_a", mlfn_arg = 555)

fn_fmls(tta(arg_a = "tta_a") %>% ttb(arg_b = "ttb_b"))
fn_fmls(tta(arg_a = "tta_a") %>% ttb(arg_a = "ttb_a", arg_b = "ttb_b"))

tta(x, arg_a = "tta_a")
tta(arg_a = "tta_a")(x)

ttd <- tta(arg_b=300)
mlstack(ttd)
ttd(x)
tte <- a() %>% ttd(arg_a = 1000)
mlstack(tte)
tte(x)

(tta(arg_a = "tta_a") %>% ttb(arg_b = "ttb_b"))(x) # nolint
(tta(arg_a = "tta_a") %>% ttb(arg_a = "ttb_a", arg_b = "ttb_b"))(x) # nolint

tt <-
    a(arg_a = 1, other = 3) %>% 
    mlfn(x, {
        mlcontinue(modifyList(x, ll(mlfn_1 = "mlfn_1")))
    }) %>%
    ml(b(arg_b = 1, other = 3),
       mlfn(x, {
           mlcontinue(modifyList(x, ll(mlfn_2 = "mlfn2")))
       }))
str(tt(2 + x, arg_a = 100, extra_arg = 100))



## split

tte <- tta() %>%
    mlsplit(nsplits = 3) %>%
    ml_log("Some message") %>%
    b(arg_b = 200) %>%
    mlfn({
        x
    })

tte(list(a = 1:100))

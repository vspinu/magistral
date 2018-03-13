
mlmap <- function(x, .f, ...) {
    if (is.function(.x))
        return(mlfunction("mlmap"))
    switch(x$op,
           describe = {
               descr <- ll(doc = "Generic Mapper.", 
                           handles = c("run", "map", "describe"),
                           responds = c("next"))
           }, 
           run =  {
               .f <- as_mapper(.f, ...)
               data <- x$data
               cont <- TRUE
               i <- 1
               N <- length(data)
               while (cont && i <= N) {
                   x <- mlcontinue(assoc(x, "data", .f(data[[i]]),
                                            "map.i", i, "map.N" = N))
                   cont <- switch(x$ret,
                                  done = TRUE,
                                  stop = FALSE,
                                  TRUE)
               }
               ml_return(x)
           },
           mlcontinue(.x))
}

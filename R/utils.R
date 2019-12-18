
## Map over names or a character vector
nmap <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  ix <-
    if (is.null(names(.x)))
      if (is.character(.x)) .x
      else seq_along(.x)
    else names(.x)
  map(set_names(ix), .f, ...)
}

rm_doted <- function(out = NULL, pattern = "^\\.") {
  env <- parent.frame()
  stopif(identical(env, .GlobalEnv))
  rm(list = ls(envir = env, all.names = TRUE, pattern = "^\\."), envir = env)
  out
}

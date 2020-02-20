
rmap <- function(.x, .fpre = NULL, .fpost = NULL, .fstop = NULL, ...) {
  .fpost <- if (is.null(.fpost)) identity
            else purrr::as_mapper(.fpost, ...)
  .fpre <- if (is.null(.fpre)) identity
           else purrr::as_mapper(.fpre, ...)
  .fstop <- if (is.null(.fstop)) function(x) FALSE
            else purr::as_mapper(.fstop = NULL)
  .path. <- character()
  worker <- function(x, nm) {
    .path. <<- c(.path., nm)
    x <- .fpre(x)
    if (is.recursive(x) && !.fstop(x)) {
      .fpost(imap(x, worker))
    } else {
      .fpost(x)
    }
  }
  .fpost(imap(.fpre(.x), worker))
}

## Map over names or a character vector
nmap <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  ix <-
    if (is.null(names(.x)))
      if (is.character(.x)) .x
      else seq_along(.x)
    else names(.x)
  ix <- set_names(ix)
  .Call(purrr:::map_impl, environment(), "ix", ".f", "list")
}


rm_doted <- function(out = NULL, pattern = "^\\.") {
  env <- parent.frame()
  stopif(identical(env, .GlobalEnv))
  rm(list = ls(envir = env, all.names = TRUE, pattern = "^\\."), envir = env)
  out
}

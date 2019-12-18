
#' @export
ml_model <- function(x, respvar, model) {
  x[["models"]][[respvar]][[model]][["model"]]
}


## Predictor

.default_predict <- function(x, split, respvar, model, ...) {
  stats::predict(ml_model(x, respvar, model), ml_data(x, split), ...)
}

ml_predict_fn <- function(x, respvar, model) {
  fn <- pluck(x, "models", respvar, model, "predict_fn",
              .default = .default_predict)
  if (is.function(fn)) fn
  else getFunction(as.character(fn))
}

#' @export
ml_predictions <- function(x, long = FALSE, extra_data_vars = NULL, extra_DATA_vars = NULL) {
  imap(x[["predictions"]], function(preds, respvar) {
    out <-
      imap_dfr(preds, function(pred, split) {
        cbind(data.table(split = split,
                         ix = x[["splits"]][[split]],
                         actual = ml_DATA(x, split, respvar)[[1]]), 
              if (long) set_names(stack(pred), c("predicted", 'model'))
              else as.data.table(pred),
              if (!is.null(extra_data_vars))
                ml_data(x, split, extra_data_vars),
              if (!is.null(extra_DATA_vars))
                ml_DATA(x, split, extra_DATA_vars))
      })
    out[, split := as.factor(split)]
    if (long)
      out[, model := as.factor(model)]
    out
  })
}

#' @export
predict.magistral.state <- function(x, splits = NULL, ...) {
  if (is.null(splits))
    splits <- names(x[["splits"]])
  if (is.null(splits)) {
    x[["splits"]][[".ALL."]] <- 1:nrow(ml_data(x))
    splits <- ".ALL."
  }
  if (length(diff <- setdiff(splits, names(x[["splits"]])))) {
    stop("Invalid splits {paste_comma(diff)}")
  }
  preds <-
    imap(x[["models"]], function(mdls, respvar) {
      nmap(splits, function(split) {
        nmap(mdls, function(model) {
          catlog("predicting '{split}:{respvar}' with '{model}' ...")
          ml_predict_fn(x, respvar, model)(x, split, respvar, model, ...)
        })
      })
    })
  list_modify(x, predictions = preds)
}

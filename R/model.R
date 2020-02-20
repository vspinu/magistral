
lst_lookup <- function(where, keys) {
  ix <- 0L
  for (el in where) {
    ix <- ix + 1L
    if (all(imap_lgl(keys, ~ identical(.x, el[[.y]])))) {
      return(list(ix, el))
    }
  }
  ## stop(sprintf("No matching %s with key (%s)", what, 
  ##              paste(names(keys), keys, sep = "=", collapse = ", ")))
  NULL
}

lst_inset <- function(where, keys, object) {
  ix <- lst_lookup(where, keys)[[1]]
  if (is.null(ix)) {
    
  }
}

#' @export
ml_raw_model <- function(x, name, var) {
  x[["models"]][[name]][["model"]]
}

#' @export
ml_inset_model <- function(x, ...) {
  obj <- structure(list2(...), class = "magistrat.model")
  list_modify(x, models = obj)
}

#' @export
is.magistral.model <- function(x) {
  inherits(x, "magistrat.model")
}

#' @export
is.magistral.prediction <- function(x) {
  inherits(x, "magistrat.prediction")
}

## ml_flatten_models <- function(x) {
##   rmap(x,
##        .fpre = ~ unlist(.x, recursive = FALSE), 
##        .fstop = is.magistral.model)
## }

rmap_object <- function(models, class, fn, flatten = FALSE) {
  rmap(models,
       .fstop = ~ inherits(.x, class),
       .fpre = function(x) {
         if (inherits(x, class)) fn(x)
         else x
       }, 
       .fpost = function(x) {
         if (flatten && !inherits(x, class))  {
           unlist(x, recursive = FALSE)
         } else {
           x
         }
       })
}


## Predictor

.default_predict <- function(x, split, model, ...) {
  stats::predict(model[["raw_model"]], ml_data(x, split), ...)
}

predict_fn <- function(model) {
  fn <- model[["predict_fn"]]
  if (is.NULL(fn))
    return(.default_predict)
  if (is.function(fn)) fn
  else getFunction(as.character(fn))
}

#' @export
ml_predictions <- function(x, long = FALSE,
                           extra_data_vars = NULL,
                           extra_DATA_vars = NULL,
                           unnest = "auto") {
  out <-
    rmap_object(x[["predictions"]], "magistral.prediction",
                flatten = TRUE, 
                function(pred_types) {
                  out <-
                    imap_dfr(pred_types, function(splits, pred_type) {
                      imap_dfr(splits, function(pred, split) {
                        cbind(data.table(split = split,
                                         type = pred_type, 
                                         ix = x[["splits"]][[split]],
                                         actual = ml_DATA(x, split, respvar)[[1]],
                                         type = attr(pred, "prediction_type"), 
                                         pred), 
                              if (!is.null(extra_data_vars))
                                ml_data(x, split, extra_data_vars),
                              if (!is.null(extra_DATA_vars))
                                ml_DATA(x, split, extra_DATA_vars))
                      })
                    })
                  out[, `:=`(split = as.factor(split),
                             type = as.factor(type))]
                  out
                })
  out
}

#' @export
predict.magistral.state <- function(x, splits = NULL, prediction_type = "default", ...) {
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
    rmap_object(x, "magistral.model", 
                function(mdl) {
                  out <-
                    nmap(splits, function(split) {
                      pred_fn <- predict_fn(mdl)
                      respvar <- mld[["respvar"]]
                      catlog("predicting '{split}:{respvar}' with '{model}' ...")
                      pred_fn(x, split, model, prediction_type, ...)
                    })
                  if (is.null(prediction_type))
                    prediction_type <- attr(out[[1]], "prediction_type")
                  if (is.null(prediction_type))
                    prediction_type <- "default"
                  structure(out,
                            prediction_type = prediction_type, 
                            respvar = respvar, 
                            class = "magistral.prediction")
                })
  list_modify(x, predictions = preds)
}

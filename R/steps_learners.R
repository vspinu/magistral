

### XGBOOST

#' @export
xgb_data <- function(x, split = NULL, respvar = NULL) {
  resp <- if (!is.null(respvar))
            as.numeric(ml_DATA(x, split, respvar)[[1]])
  mat <- numatrix(ml_data(x, split))
  xgboost::xgb.DMatrix(mat, label = resp)
}

.xgb_predict <- function(x, split, model, ...) {
  xgboost:::predict.xgb.Booster(model[["raw_model"]],
                                numatrix(ml_data(x, split)),
                                ...)
}

#' @export
xgb_pred <- function(raw_model, st, split = NULL, ...) {
  mat <- numatrix(ml_data(st, split))
  xgboost:::predict.xgb.Booster(raw_model, newdata = mat, ...)
}

#' @export
xgb_train <- function(x, respvar, params, test_split = "NOTRAIN", nrounds = 1500,
                      name = "xgb", free_data = TRUE, train_split = "train") {
  library(xgboost)
  stopifnot(length(train_split) == 1)
  if (identical(test_split, "ALL"))
    test_split <- names(x[["splits"]])
  if (identical(test_split, "NOTRAIN"))
    test_split <- setdiff(names(x[["splits"]]), train_split)
  devdsets <- nmap(test_split, ~ xgb_data(x, .x, respvar))
  traindset <- if (train_split %in% names(devdsets)) devdsets[[train_split]]
               else xgb_data(x, train_split, respvar)
  faw_model <-
    xgb.train(data = traindset, 
              watchlist = devdsets, 
              params = params,
              verbose = 1, 
              print_every_n = 25,
              nrounds = nrounds)
  ml_inset_model(x, name,
                 raw_model = raw_model,
                 respvar = respvar, 
                 predict_fn = ".xgb_predict")
}


### LIGHTGBM

#' @export
lgb_data <- function(x, split = NULL, respvar = NULL) {
  library(lightgbm)
  cats <- which(map_lgl(ml_data(x), is.factor))
  resp <- if (!is.null(respvar)) as.numeric(ml_DATA(x, split, respvar)[[1]])
  mat <- numatrix(ml_data(x, split))
  lightgbm::lgb.Dataset(mat, label = resp, categorical_feature = cats)
}

.lgb_predict <- function(x, split, respvar, model, ...) {
  lightgbm:::predict.lgb.Booster(model[["raw_model"]],
                                 ml_lgb_data(x, split), ...)
}

#' @export
lgb_train <- function(x, respvar, params, test_split = "NOTRAIN", nrounds = 1500,
                      name = "lgb", free_data = TRUE, train_split = "train") {
  stopifnot(length(train_split) == 1)
  if (identical(test_split, "ALL"))
    test_split <- names(x[["splits"]])
  if (identical(test_split, "NOTRAIN"))
    test_split <- setdiff(names(x[["splits"]]), train_split)
  devdsets <- nmap(test_split, ~ ml_lgb_data(x, .x, respvar))
  traindset <- if (train_split %in% names(devdsets)) devdsets[[train_split]]
               else ml_lgb_data(x, train_split, respvar)
  raw_model <-
    lgb.train(data = traindset, params = params, verbose = 1, eval_freq = 25,
              valids = devdsets, nrounds = nrounds, reset_data = free_data)
  ml_inset_model(x, name,
                 raw_model = raw_model,
                 respvar = respvar, 
                 predict_fn = ".lgb_predict")
}


### CATBOOST

#' @export
ml_cgb_data <- function(x, split, respvar = NULL) {
  resp <- if (!is.null(respvar)) as.numeric(ml_DATA(x, split, respvar)[[1]])
  catboost.load_pool(ml_data(x, split), label = resp)
}

.cgb_predict <- function(x, split, respvar, model,
                         prediction_type = "Probability", ...) {
  catboost.predict(model[["raw_model"]],
                   ml_cgb_data(x, split),
                   prediction_type = prediction_type, 
                   ...)
}

#' @export
cgb_train <- function(x, respvar, params, nrounds = 1500,
                      test_split = NULL, train_split = "train",
                      name = "cgb") {
  library(catboost)
  stopifnot(train_split %in% names(x[["splits"]]))
  testset <-
    if (!is.null(test_split))
      ml_cgb_data(x, test_split, respvar)
  trainset <-
    if (train_split == test_split) testset
    else ml_cgb_data(x, train_split, respvar)
  params <-
    list_modify(list(iterations = nrounds,
                     name = name, 
                     logging_level = "Verbose",
                     metric_period = 25,
                     train_dir = "catboost"),
                !!!params)
  raw_model <-
    catboost.train(learn_pool = trainset, 
                   test_pool = testset,
                   params = params)
  ml_inset_model(x, name,
                 raw_model = raw_model,
                 respvar = respvar, 
                 predict_fn = ".cgb_predict")
}

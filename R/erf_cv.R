erf_cv <- function(X,
                   Y,
                   min.node.size = c(5, 40, 100),
                   lambda = c(0, 0.001, 0.01),
                   intermediate_estimator = c("grf", "neural_nets"),
                   intermediate_quantile = 0.8,
                   nfolds = 5, nrep = 3, seed = NULL,
                   ...) {

  # validate stuff ...

  # fit and predict intermediate estimator
  intermediate_threshold <- fit_intermediate_threshold(
    X, Y,
    intermediate_estimator,
    ...
  )

  Q_X <- predict_intermediate_quantile(
    intermediate_threshold,
    intermediate_quantile = intermediate_quantile,

  )

  # call constructor for erf_cv
  fit_erf_cv(...)

}

fit_erf_cv <- function(...){
  # create splits
  splits <- repeated_k_folds(n, nfolds, nrep, seed)

  # create parameter grid
  params <- param_grid(splits, min.node.size, lambda, nfolds, nrep)

  # call fit_and_score for each fold and each parameter setting
  evaluate_candidates(X, Y, Q_X, params)
}

evaluate_candidates <- function(X, Y, intermediate_threshold, params) {
  ## numeric_matrix numeric_vector numeric_matrix tibble -> tibble
  ## return tibble with scores !!!

  fit_and_score_partial <- purrr::partial(
    fit_and_score,
    X = X,
    Y = Y,
    intermediate_threshold = intermediate_threshold
  )

  purrr::pmap(params, fit_and_score_partial)
}

fit_mini_grf <- function(X, Y, min.node.size) {
  ## numeric_matrix numeric_vector numeric -> mini_forest (grf + Q_X)
  ## fits a small quantile forest with 50 trees

  grf::quantile_forest(X, Y, min.node.size = min.node.size, num.trees = 50)
}

score_mini_grf <- function(mini_forest, X_test, Y_test, Q_X_test, lambda) {
  ## mini_forest numeric_matrix numeric_vector numeric_matrix numeric
  ## -> numeric
  ## evaluate deviance score for fitted quantile_forest

  # predict parameters
  gpd_pars <- predict_gpd_params(
    quantile_forest = mini_forest$quantile_forest,
    newdata = X_test, # !!! only on exceedances, i.e., Y_test > Q_X_test
    Q = mini_forest$Q_X,
    lambda = lambda
  )

  # compute deviance
  evaluate_deviance(
    gpd_pars,
    Y_test,
    Q_X_test
  )


}

fit_and_score <- function(X, Y, Q_X, folds,
                          min.node.size, lambda){
  # split X and Y
  X_test <-  Y_test <-  ... <- ...

  # estimator <- fit(X_train, Y_train, fit_params)
  erf_obj <- fit_erf(
    X = X_train,
    Y = Y_train,
    min.node.size = min.node.size,
    lambda = lambda,
    num.trees = 50)

  # score estimator
  test_scores <- score(erf_obj, X_test, Y_test)

}


create_folds <- function(n, n_rep, K) {
  ## integer (4x) -> list
  ## produce a list with n_rep splits for K-fold CV

  rows_id <- 1:n

  lapply(X = rep(1, n_rep), FUN = function(x) {
    chunk(sample(rows_id), K)
  })
}

chunk <- function(x, K) {
  ## numeric_vector integer -> list
  ## split x into K chunks
  unname(split(x, factor(sort(rank(x) %% K))))
}

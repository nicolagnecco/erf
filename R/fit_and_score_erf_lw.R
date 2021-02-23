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

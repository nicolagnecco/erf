fit_and_score_erf_lw <- function(X, Y, Q_X, folds,
                          min.node.size, lambda, intermediate_quantile){
  ## numeric_matrix numeric_vector numeric_vector numeric (3x) -> numeric
  ## fit a light-weight erf and computes its cross validation error

  # split X, Y, Q_X
  X <- split_data(X, folds)
  Y <- split_data(Y, folds)
  Q_X <- split_data(Q_X, folds)

  # fit light-weight erf
  fitted_erf_lw <- fit_mini_erf_lw(X$train, Y$train, Q_X$train,
                              min.node.size, lambda, intermediate_quantile)

  # keep only test observations where Y > Q_X
  exc_id <- Y$test > Q_X$test
  X$test <- X$test[exc_id, , drop = FALSE]
  Y$test <- Y$test[exc_id]
  Q_X$test <- Q_X$test[exc_id]

  # predict gpd parameters
  gpd_params <- predict_gpd_params(fitted_erf_lw, X$test)

  # evaluate deviance
  if (length(Y$test) == 0){
    return(10 ^ 6) # !!! ask Seb
  } else {
    evaluate_deviance(gpd_params, Y$test, Q_X$test)
  }
}

erf_cv <- function(X,
                   Y,
                   min.node.size = c(5, 40, 100),
                   lambda = c(0, 0.001, 0.01),
                   intermediate_estimator = c("grf", "neural_nets"),
                   nfolds = 5, nrep = 3, seed = NULL,
                   ...) {

  # validate stuff ...

  # fit intermediate estimator

  # split data
  # call fit_and_score for each fold and each parameter setting

  # for each fold call: fit_and_score(X, Y, train, test, fit_params, ...)
  # 1. split X and Y
  # 2. estimator <- fit(X_train, Y_train, fit_params)
  # 3. test_scores <- score(estimator, X_test, Y_test)

  # Run jobs
  # args: X, Y, train, test, min.node.size, ...



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

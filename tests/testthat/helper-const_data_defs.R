#' Constants:

n <- 1e3
n_small <- 500
p_small <- 2
p <- 10
n_test <- 1e3
min.node.size <- 40
lambda <- 0.001
df <- 4



#' Data definitions:

#' `X` is `numeric_matrix` with `n` rows and `p` columns
#' interp. matrix with `n` observations with `p` predictors
X <- matrix(runif(n * p, min = -1, max = 1), nrow = n, ncol = p)
X_test <- matrix(runif(n_test * p, min = -1, max = 1), nrow = n_test, ncol = p)
X_small <-  matrix(runif(n_small * p_small, min = -1, max = 1),
                   nrow = n_small, ncol = p_small)


#' `Y` is `numeric_vector` with `n` elements
#' interp. vector of responses
Y <- (1 + 1 * (X[, 1] > 0)) * rt(n, df)
Y_test <- (1 + 1 * (X_test[, 1] > 0)) * rt(n_test, df)
Y_small <- (1 + 1 * (X_small[, 1] > 0)) * rt(n_small, df)


#' `quantile_forest` is an object from `grf::quantile`
#' interp. a quantile forest
quantile_forest_1 <- grf::quantile_forest(X, Y, min.node.size = min.node.size)


#' `intermediate_estimator` is one of:
#' - `"grf"`
#' - `"neural_nets"`
#' interp. type of estimator used to fit the intermediate threshold.


#' `intermediate_threshold` is one of
#' - `"quantile_forest"`, if `intermediate_threshold = "grf"`.
#' - `"..."`, if `intermediate_threshold = "neural_nets"`.
#' interp. a fitted object to predict intermediate thresholds.
intermediate_threshold_1 <- grf::quantile_forest(X, Y)
intermediate_threshold_2 <- structure(list(), class = "quantile_forest")


structure(list(), class = "extreme_forest")
#' `extreme_forest` is a named list made of:
#' - `quantile_forest` is `quantile_forest`.
#' - `min.node.size` is numeric.
#' - `lambda` is numeric.
#' - `intermediate_threshold` is `intermediate_threshold`.
#' interp. an extreme forest.
extreme_forest_1 <- structure(list(
  "quantile_forest" = quantile_forest_1,
  "min.node.size" = min.node.size,
  "lambda" = lambda,
  "intermediate_threshold" = intermediate_threshold_1),
class = "extreme_forest"
)


structure(list(), class = "extreme_forest_cv")
#'' `extreme_forest_cv` is a named list made of:
#'' * `scores`: a `tibble` with columns: `min.node.size`, `lambda`, `cvm`
#''  (mean cross-validated error).
#'' * `extreme_forest.fit`: a fitted "`extreme_forest`" object on the full data.
#'' interp. a cross-validated extreme forest.
extreme_forest_cv_1 <- list(
  "scores" = tibble::tibble(
    min.node.size = c(5, 40),
    lambda = c(0, 0.001),
    cvm = c(40, 36)
  ),
  "extreme_forest.fit" = extreme_forest_1
)

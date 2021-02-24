#' Constants:

n <- 200
p <- 6
n_small <- 70
p_small <- 2
n_test <- 150
min.node.size <- 40
lambda <- 0.001
df <- 4
intermediate_quantile <- 0.85
quantiles <- c(0.85, 0.9, 0.99, 0.995, 0.999, 0.9995, 0.9999)



#' Data definitions:

#' `X` is `numeric_matrix` with `n` rows and `p` columns
#' interp. matrix with `n` observations with `p` predictors
X <- matrix(runif(n * p, min = -1, max = 1), nrow = n, ncol = p)
X_test <- matrix(runif(n_test * p, min = -1, max = 1),
                 nrow = n_test, ncol = p)
X_small <- matrix(runif(n_small * p_small, min = -1, max = 1),
                  nrow = n_small, ncol = p_small
)
X_test_small <- matrix(runif(n_test * p_small, min = -1, max = 1),
                       nrow = n_test, ncol = p_small)


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
intermediate_estimator_1 <- "grf"
intermediate_estimator_2 <- "neural_nets"


#' `intermediate_threshold` is one of
#' - `"quantile_forest"`, if `intermediate_estimator = "grf"`.
#' - `"???"`, if `intermediate_estimator = "neural_nets"`.
#' - other S3 class with `predict` method.
#' interp. a fitted object to predict intermediate thresholds.
intermediate_threshold_1 <- grf::quantile_forest(X, Y)
intermediate_threshold_1_small <- grf::quantile_forest(X, Y, num.trees = 50)
intermediate_threshold_2 <- fit_intermediate_threshold(X_small, Y_small,
                                                       estimator = "grf")
intermediate_threshold_3 <- structure(list(), class = "quantile_forest")
intermediate_threshold_4 <- lm(Y ~ ., data = data.frame(X_small,
                                                        Y = Y_small))

#' `Q_X` is numeric vector
#' interp. predicted quantile at some intermediate level
Q_X <- predict_intermediate_quantile(
  intermediate_threshold_1,
  intermediate_quantile = intermediate_quantile
)


structure(list(), class = "erf")
#' `erf` is a named list made of:
#' - `quantile_forest` is `quantile_forest`.
#' - `min.node.size` is numeric.
#' - `lambda` is numeric.
#' - `intermediate_threshold` is `intermediate_threshold`.
#' - `intermediate_quantile` is numeric.
#' - `Q_X` is numeric vector.
#' interp. an extreme forest.
erf_1 <- structure(list(
  "quantile_forest" = quantile_forest_1,
  "min.node.size" = min.node.size,
  "lambda" = lambda,
  "intermediate_threshold" = intermediate_threshold_1,
  "intermediate_quantile" = intermediate_quantile,
  "Q_X" = Q_X
),
class = "erf"
)

erf_2 <- erf(X_small, Y_small,
             intermediate_estimator = "grf",
             intermediate_quantile = intermediate_quantile)

Q_X_small <- predict_intermediate_quantile(
  intermediate_threshold = erf_2$quantile_forest,
  intermediate_quantile = erf_2$intermediate_quantile
)

erf_3 <- structure(list(
  "quantile_forest" = erf_2$quantile_forest,
  "min.node.size" = erf_2$min.node.size,
  "lambda" = erf_2$lambda,
  "intermediate_threshold" = erf_2$quantile_forest,
  "intermediate_quantile" = erf_2$intermediate_quantile,
  "Q_X" = Q_X_small
),
class = "erf"
)


structure(list(), class = "erf_cv")
#' `erf_cv` is a named list made of:
#' * `scores`: a `tibble` with columns: `min.node.size`, `lambda`, `cvm`
#'  (mean cross-validated error).
#' * `erf.fit`: a fitted "`erf`" object on the full data.
#' interp. a cross-validated extreme forest.
erf_cv_1 <- list(
  "scores" = tibble::tibble(
    "min.node.size" = c(5, 40),
    "lambda" = c(0, 0.001),
    "cvm" = c(40, 36)
  ),
  "erf.fit" = erf_1
)
# !!! to decide

structure(list(), class = "erf_lw")
#' `erf_lw` is a named list made of:
#' - `quantile_forest` is `quantile_forest`.
#' - `min.node.size` is numeric.
#' - `lambda` is numeric.
#' - `intermediate_quantile` is numeric.
#' - `Q_X` is numeric vector.
#' interp. a light-weight extreme forest, without the `intermediate_threshold`.
erf_lw_1 <- structure(list(
  "quantile_forest" = quantile_forest_1,
  "min.node.size" = min.node.size,
  "lambda" = lambda,
  "intermediate_quantile" = 0.8,
  "Q_X" = Q_X
),
class = "erf_lw"
)


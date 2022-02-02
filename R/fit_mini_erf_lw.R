fit_mini_erf_lw <- function(X, Y, Q_X,
                            min.node.size = 5, lambda = 0.001,
                            intermediate_quantile = 0.8,
                            num.trees = 50) {
  ## numeric_matrix numeric_vector (2x) numeric (3x) -> erf_lw
  ## fits a light-weight erf with `num.trees` trees

  # fit **mini** generalized random forest
  extreme_quantile_fit <- grf::quantile_forest(
    X = X, Y = Y,
    min.node.size = min.node.size,
    num.trees = num.trees
  )

  # return erf_lw object
  structure(list(
    "quantile_forest" = extreme_quantile_fit,
    "min.node.size" = min.node.size,
    "lambda" = lambda,
    "intermediate_quantile" = intermediate_quantile,
    "Q_X" = Q_X
  ),
  class = "erf_lw"
  )
}

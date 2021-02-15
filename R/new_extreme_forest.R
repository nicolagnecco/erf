


new_extreme_forest <- function(X, Y, min.node.size, lambda,
                               intermediate_estimator) {
  ## numeric_matrix numeric_vector numeric numeric intermediate_estimator
  ## -> extreme_forest
  ## fits an extreme_forest

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  # fit generalized random forest
  extreme_quantile_fit <- grf::quantile_forest(
    X = X, Y = Y,
    min.node.size = min.node.size
  )

  # return extreme_forest object
  structure(list(
    "quantile_forest" = structure(list(), class = "quantile_forest"),
    min.node.size = min.node.size,
    lambda = lambda,
    "intermediate_estimator" = structure(list(), class = "quantile_forest")
  ),
  class = "extreme_forest"
  )
}

fit_intermediate_threshold <- function(X, Y, estimator, ...) {
  ## numeric_matrix numeric_vector intermediate_estimator dots ->
  ## intermediate_estimator
  ## return intermediate_threshold

  if (estimator == "grf") {
    return(grf::quantile_forest(X, Y, ...))
  } else if (estimator == "neural_nets") {
    abort_not_implemented(estimator) # !!! add code for neural_nets
  } else {
    abort_not_implemented(estimator)
  }
}

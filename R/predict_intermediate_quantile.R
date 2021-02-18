predict_intermediate_quantile <- function(intermediate_threshold,
                                          newdata,
                                          quantile_intermediate,
                                          ...) {
  ## intermediate_threshold numeric_matrix numeric dots -> numeric_vector
  ## predict intermediate quantiles given an intermediate_threshold object
  if (inherits(intermediate_threshold, "quantile_forest")) {

    predict(
      object = intermediate_threshold,
      newdata = newdata,
      quantiles = quantile_intermediate
    )

  } else {

    predict(
      object = intermediate_threshold,
      newdata = newdata,
      ...
    )

  }
}

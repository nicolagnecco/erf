predict_intermediate_threshold <- function(intermediate_threshold,
                                           newdata = NULL,
                                           quantile_intermediate,
                                           ...) {
  ## intermediate_threshold numeric_matrix numeric dots -> numeric_matrix
  ## predict intermediate quantiles given an intermediate_threshold object
  if (inherits(intermediate_threshold, "quantile_forest")) {

    stats::predict(
      object = intermediate_threshold,
      newdata = newdata,
      quantiles = quantile_intermediate
    )

  } else {

    as.matrix(
      stats::predict(
        object = intermediate_threshold,
        newdata = newdata,
        ...
      ))

  }
}

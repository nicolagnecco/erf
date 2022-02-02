predict_intermediate_quantile <- function(intermediate_threshold,
                                           newdata = NULL,
                                           intermediate_quantile,
                                           ...) {
  ## intermediate_threshold numeric_matrix numeric dots -> numeric_vector
  ## predict intermediate quantiles given an intermediate_threshold object

  if (inherits(intermediate_threshold, "quantile_forest")) {

    unlist(
      stats::predict(
        object = intermediate_threshold,
        newdata = newdata,
        quantiles = intermediate_quantile
      )
    )

  } else if (inherits(intermediate_threshold, "neural_nets")) {
    abort_not_implemented(type = "S3 class",
                          name = class(intermediate_threshold)[1],
                          fun_name = rlang::call_name(match.call()))
    # !!! add code for neural_nets
  } else {
    abort_not_implemented(type = "S3 class",
                          name = class(intermediate_threshold)[1],
                          fun_name = rlang::call_name(match.call()))
  }

}

#' Fit an extremal random forest (ERF)
#'
#'
#' Internal function to fit an extremal random forest ... !!! write description
#'
#'
#' !!! write details
#'
#' @inheritParams erf
#'
#' @inherit erf return
#'
#' @examples
#' "!!! add examples"
#'
#' @noRd
fit_erf <- function(X, Y, min.node.size = 5, lambda = 0.001,
                    intermediate_estimator = c("grf", "neural_nets"),
                    intermediate_quantile = 0.8,
                    num_threads = NULL) {
  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  # fit intermediate quantile estimator
  intermediate_threshold <- fit_intermediate_threshold(
    X, Y,
    intermediate_estimator)

  # predict intermediate_threshold Q_X
  Q_X <- predict_intermediate_quantile(
    intermediate_threshold = intermediate_threshold,
    intermediate_quantile = intermediate_quantile
  )

  # fit generalized random forest
  extreme_quantile_fit <- grf::quantile_forest(
    X = X, Y = Y,
    min.node.size = min.node.size,
    num.threads = num_threads
  )

  # return erf object
  structure(list(
    "quantile_forest" = extreme_quantile_fit,
    "min.node.size" = min.node.size,
    "lambda" = lambda,
    "intermediate_threshold" = intermediate_threshold,
    "intermediate_quantile" = intermediate_quantile,
    "Q_X" = Q_X
  ),
  class = "erf"
  )
}

#' Fit an extremal random forest (ERF)
#'
#'
#' Internal function to fit an extremal random forest ... !!! write description
#'
#'
#' !!! write details
#'
#'
#' @param X Numeric matrix (or `data.frame`) of predictors, where each row
#'  corresponds to an observation and each column to a predictor.
#'
#' @param Y Numeric vector of responses.
#'
#' @param min.node.size Minimum number of observations in each tree leaf used
#'  in [grf::quantile_forest()].
#'  Nodes with size smaller than `min.node.size` can occur,
#'  as in the original \pkg{randomForest} package.
#'  Default is 5.
#'
#' @param lambda Penalty for the shape parameter used in the weighted likelihood.
#'  Default is 0.001.
#'
#' @param intermediate_estimator A character specifying the estimator used to
#'  fit the intermediate threshold.
#'  Options available are:
#'  - `grf`, see [grf::quantile_forest()].
#'  - `neural_nets`, (coming soon).
#'
#'  @param intermediate_quantile Intermediate quantile
#'  level, used to predict the intermediate threshold.
#'  For further information see \insertCite{merg2020;textual}{erf}.
#'
#'
#' @return An object with S3 class "`erf`".
#'  It is a named list with the following elements:
#'
#'  \item{quantile_forest}{An object with S3 class "`quantile_forest`" (see
#'   [grf::quantile_forest()]). This is a fitted generalized random forest that
#'   contains the similarity weights used in the log-likelihood.}
#'
#'  \item{min.node.size}{Minimum number of observations in each tree leaf used
#'   to fit the `quantile_forest`.}
#'
#'  \item{lambda}{Penalty for the shape parameter used in the weighted
#'  log-likelihood.}
#'
#'  \item{intermediate_threshold}{An object with S3 class:
#'
#'  - "`quantile_forest`", if `intermediate_estimator = "grf"`.
#'  - "`???`", if `intermediate_estimator = "neural_nets`.
#'
#'  This is a fitted object used to predict the intermediate thresholds.}
#'
#'  \item{intermediate_quantile}{Intermediate quantile
#'  level, used to predict the intermediate threshold.}
#'
#'  \item{Q_X}{Vector with intermediate quantile predicted on the training data
#'  `X`.}
#'
#'
#' @examples
#' "!!! add examples"
fit_erf <- function(X, Y, min.node.size, lambda,
                    intermediate_estimator,
                    intermediate_quantile) {
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
  Q_X <- predict_intermediate_threshold(
    intermediate_threshold = intermediate_threshold,
    intermediate_quantile = intermediate_quantile
  )

  # fit generalized random forest
  extreme_quantile_fit <- grf::quantile_forest(
    X = X, Y = Y,
    min.node.size = min.node.size
  )

  # return erf object
  structure(list(
    "quantile_forest" = extreme_quantile_fit,
    min.node.size = min.node.size,
    lambda = lambda,
    "intermediate_threshold" = intermediate_threshold,
    "intermediate_quantile" = intermediate_quantile,
    "Q_X" = Q_X
  ),
  class = "erf"
  )
}

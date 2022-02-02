#' Fit an extremal random forest (ERF)
#'
#'
#' Fits an extremal random forest (ERF).
#'
#'
#' @param X Numeric matrix of predictors, where each row
#'  corresponds to an observation and each column to a predictor.
#'
#' @param Y Numeric vector of responses.
#'
#' @param min.node.size Minimum number of observations in each tree
#'  leaf used to fit the similarity weights
#'  (see also [grf::quantile_forest()]).
#'  Nodes with size smaller than `min.node.size` can occur,
#'  as in the original \pkg{randomForest} package.
#'  Default is `5`.
#'
#' @param lambda Penalty for the shape parameter used in the weighted likelihood.
#'  Default is `0.001`.
#'
#' @param intermediate_estimator A character specifying the estimator used to
#'  fit the intermediate threshold.
#'  Options available are:
#'  - `grf`, see [grf::quantile_forest()].
#'  - `neural_nets`, (coming soon).
#'
#' @param intermediate_quantile Intermediate quantile
#'  level, used to predict the intermediate threshold.
#'  For further information see \insertCite{merg2020;textual}{erf}.
#'  Default is `0.8`.
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
#'  - "`neural_nets`", if `intermediate_estimator = "neural_nets` (*note*:
#'      this option is not yet implemented)
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
#' @export
erf <- function(X, Y, min.node.size = 5, lambda = 0.001,
                intermediate_estimator = c("grf", "neural_nets"),
                intermediate_quantile = 0.8) {

  # validate inputs
  validate_data(X, Y)

  validate_params(min.node.size, lambda)

  intermediate_estimator <- validate_intermediate_estimator(
    intermediate_estimator
  )

  validate_intermediate_quantile(intermediate_quantile)

  # return erf object
  validate_erf(fit_erf(
    X, Y, min.node.size, lambda,
    intermediate_estimator,
    intermediate_quantile
  ))
}

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
#' @param intermediate_threshold An object with arbitrary S3 class that
#'  possesses a `predict` method.
#'  Such "`predict`" method must accept a numeric matrix (or `data.frame`)
#'  of test predictors, e.g., `newdata`, as **second** argument.
#'  For example, `predict(intermediate_threshold, newdata, ...)`
#'  must execute with no errors.
#'
#'  This is a fitted object used to predict the intermediate thresholds.
#'
#'
#' @return An object with S3 class "`extreme_forest`".
#'  It is a named list with the following elements:
#'
#'  \item{quantile_forest}{An object with S3 class "`quantile_forest`" (see
#'    [grf::quantile_forest()]). This is a fitted generalized random forest that
#'    contains the similarity weights used in the log-likelihood.}
#'
#'  \item{min.node.size}{Minimum number of observations in each tree leaf used
#'    to fit the `quantile_forest`.}
#'
#'  \item{lambda}{Penalty for the shape parameter used in the weighted
#'   log-likelihood.}
#'
#'  \item{intermediate_threshold}{An object with arbitraty S3 class, as
#'   described in the **Arguments** section.
#'   It is a fitted object used to predict the intermediate thresholds.}
#'
#'
#' @examples
#' "!!! add examples"
new_extreme_forest <- function(X, Y, min.node.size, lambda,
                               intermediate_threshold) {
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
    "quantile_forest" = extreme_quantile_fit,
    min.node.size = min.node.size,
    lambda = lambda,
    "intermediate_threshold" = intermediate_threshold
  ),
  class = "extreme_forest"
  )
}

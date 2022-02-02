#' Cross-validation for an extremal random forest (ERF)
#'
#'
#' Fits an extremal random forest (ERF) with cross-validation.
#'
#' @inheritParams erf
#'
#' @param min.node.size Vector with minimum number of observations in each tree
#'  leaf used to fit the similarity weights
#'  (see also [grf::quantile_forest()]).
#'  Nodes with size smaller than `min.node.size` can occur,
#'  as in the original \pkg{randomForest} package.
#'  Default is `c(5, 40, 100)`.
#'
#' @param lambda Vector with penalties for the shape parameter used in the weighted likelihood.
#'  Default is `c(0, 0.001, 0.01)`.
#'
#' @param nfolds Number of folds in the cross-validation scheme.
#'  Default is `5`.
#'
#' @param nreps Number of times `nfolds` cross-validation is repeated.
#'  Default is `3`.
#'
#' @param seed Random seed to reproduce the fold splits.
#' Default is `NULL`.
#'
#'
#' @return An object with S3 class "`erf_cv`".
#'  It is a named list with the following elements:
#'
#'  \item{scores}{A `tibble` with columns: `min.node.size`, `lambda`, `cvm`
#'  (mean cross-validated error).}
#'
#'  \item{erf}{A fitted "`erf`" object on the full data using the optimal
#'  `min.node.size` and `lambda`.}
#'
#' @export
erf_cv <- function(X,
                   Y,
                   min.node.size = c(5, 40, 100),
                   lambda = c(0, 0.001, 0.01),
                   intermediate_estimator = c("grf", "neural_nets"),
                   intermediate_quantile = 0.8,
                   nfolds = 5, nreps = 3, seed = NULL) {

  # validate inputs
  validate_data(X, Y)

  validate_params(min.node.size, lambda)

  intermediate_estimator <- match.arg(intermediate_estimator)
  validate_intermediate_estimator(intermediate_estimator)

  validate_intermediate_quantile(intermediate_quantile)

  # fit erf_cv
  validate_erf_cv(fit_erf_cv(X, Y, min.node.size, lambda,
                             intermediate_estimator, intermediate_quantile,
                             nfolds, nreps, seed))
}

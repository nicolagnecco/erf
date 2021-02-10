#' Fit an extremal random forest (ERF)
#'
#' Fit an extremal random forest ... !!! write description
#'
#' !!! write details
#'
#' @param X \code{numeric matrix} --- Matrix of predictors, where each row corresponds to
#'  an observation and each column to a predictor.
#' @param Y (numeric vector) response vector.
#' @param min.node.size \code{numeric >= 0} --- A target for the minimum
#'  number of observations in each tree leaf used in [grf::quantile_forest()].
#'  Nodes with size smaller than \code{min.node.size} can occur,
#'  as in the original \pkg{randomForest} package.
#'  Default is 5.
#' @param lambda (numeric \eqn{\geq}{>=} 0)
#' @param intermediate_estimator !!! document this
#' @param ... !!! document this
#'
#' @return An object with S3 class "\code{extreme_forest}",
#' that is a named list with the following elements.
#'
#' \item{quantile_forest}{An object with S3 class "\code{quantile_forest}". See
#' [grf::quantile_forest()].}
#'
#' \item{intermediate_estimator}{An object with arbitrary S3 class that possesses the
#' "\code{predict}" method. Such "\code{predict}" method must accept
#' as **second** argument a numeric matrix with the test predictors, e.g.,
#' \code{newdata}.
#' For example, \code{predict(intermediate_estimator, newdata, ...)}
#' must execute with no errors.}
#'
#' \item{min.node.size}{!!! document this}
#'
#' \item{lambda}{!!! document this}
#'
#' @examples
#' "!!! add examples"
#'
#'
extreme_forest <- function(X, Y, min.node.size, lambda,
                           intermediate_estimator, ...){

  # validate inputs
  validate_data(X, Y)

  validate_params(min.node.size, lambda)

  validate_estimator(intermediate_estimator, ...)

  # return extreme_forest object
  validate_extreme_forest(new_extreme_forest(X, Y, min.node.size, lambda,
                                             intermediate_estimator, ...))
}


# WISHLIST
# validate_data
# validate_params
# validate_estimator
# validate_extreme_forest
# new_extreme_forest

new_extreme_forest <- function(X, Y, min.node.size, lambda,
                               intermediate_estimator, ...){
  ## numeric_matrix numeric_vector numeric numeric intermediate_estimator dots
  ## -> extreme_forest
  ## fits an extreme_forest

  # fit intermediate threshold estimator
  intermediate_estimator <- fit_intermediate_quantile(X, Y,
                                                   intermediate_estimator, ...)

  # fit generalized random forest
  extreme_quantile_fit <- grf::quantile_forest(X = X, Y = Y,
                                               min.node.size = min.node.size)

  # return extreme_forest object
  extreme_forest_1
}


# Ideal usage of the package
# 1. Fit extreme_forest
# erf.fit <- extreme_forest(X, Y, min.node.size = 40, lambda = 0.001, base_threshold = NULL, ...)
#
# predict(erf.fit, newdata = X_test, quantile_levels = c(0.999), ...) # ... any argument for predict(base_threshold, ...) e.g., base_levels = 0.8
#
# plot(erf.fit) # diagnostic plot
# print(erf.fit) # variable importance
#

# 2. CV extreme_forest
# erf.fit.cv <- extreme_forest_cv(X, Y,
#                   min.node.size = c(5, 40, 100),
#                   lambda = c(0, 0.001, 0.1, 1),
#                   nfolds = 5, nrep = 3,
#                   base_threshold = NULL,
#                   ...)
#
# predict(erf.fit.cv, newdata = X_test, quantile_levels = c(0.999), ...)
#
# plot(erf.fit.cv) # ???
# print(erf.fit.cv) # ???
#

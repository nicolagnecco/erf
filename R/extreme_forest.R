#' Fit an extremal random forest (ERF)
#'
#'
#' Fit an extremal random forest ... !!! write description
#'
#'
#' !!! write details
#'
#'
#' @param X Numeric matrix of predictors, where each row corresponds to
#'  an observation and each column to a predictor.
#'
#' @param Y Numeric response vector.
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
#' @param intermediate_estimator An object with arbitrary S3 class that possesses
#'  a `predict` method or `NULL`.
#'  If `NULL`, a quantile forest with default arguments is fitted by calling
#'  `grf::quantile_forest(X, Y)` from \pkg{grf} package.
#'  Default is `NULL`.
#'
#'
#' @return An object with S3 class "`extreme_forest`".
#'  It is a named list with the following elements:
#'
#'  \item{quantile_forest}{An object with S3 class "`quantile_forest`". See
#'   [grf::quantile_forest()].}
#'
#'  \item{intermediate_estimator}{An object with arbitrary S3 class that possesses
#'   a `predict` method.
#'   Such `predict`" method must accept a numeric matrix with the test
#'   predictors, e.g., `newdata`, as **second** argument.
#'   For example, `predict(intermediate_estimator, newdata, ...)`
#'   must execute with no errors.}
#'
#' \item{min.node.size}{Minimum number of observations in each tree leaf used
#'  in [grf::quantile_forest()].}
#'
#' \item{lambda}{Penalty for the shape parameter used in the weighted likelihood.}
#'
#'
#' @examples
#' "!!! add examples"
#'
extreme_forest <- function(X, Y, min.node.size = 5, lambda = 0.001,
                           intermediate_estimator = NULL) {

  # validate inputs
  validate_data(X, Y)

  validate_params(min.node.size, lambda)

  validate_estimator(intermediate_estimator)

  # return extreme_forest object
  validate_extreme_forest(new_extreme_forest(
    X, Y, min.node.size, lambda,
    intermediate_estimator
  ))
}


validate_data <- function(X, Y){
  ## numeric_matrix numeric_vector -> invisible(list)
  ## checks whether the given data are well formed, throws error if not

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  invisible(list(X, Y))
}


validate_params <- function(min.node.size, lambda) {
  ## numeric numeric -> invisible(list)
  ## checks whether the given parameters are well formed, throws error if not

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  invisible(list(min.node.size, lambda))

}


validate_estimator <- function(estimator){
  ## intermdiate_estimator -> invisible(intermediate_estimator)
  ## checks if the intermediate_estimator is well formed, throws error if not

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  invisible(estimator)
}


validate_extreme_forest <- function(ef){
  ## extreme_forest -> extreme_forest
  ## returns ef if it is well formed, throws error if not

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  return(ef)
}


new_extreme_forest <- function(X, Y, min.node.size, lambda,
                               intermediate_estimator, ...) {
  ## numeric_matrix numeric_vector numeric numeric intermediate_estimator dots
  ## -> extreme_forest
  ## fits an extreme_forest

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  # fit intermediate quantile estimator
  intermediate_estimator <- fit_intermediate_quantile(
    X, Y,
    intermediate_estimator
  )

  # fit generalized random forest
  extreme_quantile_fit <- grf::quantile_forest(
    X = X, Y = Y,
    min.node.size = min.node.size
  )

  # return extreme_forest object
  structure(list(
    "quantile_forest" = structure(list(), class = "quantile_forest"),
    "intermediate_estimator" = structure(list(), class = "lm"),
    min.node.size = min.node.size,
    lambda = lambda),
    class = "extreme_forest")
}


fit_intermediate_quantile <- function(X, Y, intermediate_estimator){
  ## numeric_matrix numeric_vector intermediate_estimator|NULL ->
  ## intermediate_estimator
  ## return intermediate_estimator or, if NULL, fits quantile forest

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  return(structure(list(), class = "quantile_forest"))
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

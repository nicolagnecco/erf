#' Fit an extremal random forest (ERF)
#'
#'
#' Fit an extremal random forest ... !!! write description
#'
#'
#' !!! write details
#'
#'
#' @param X Numeric matrix or data.frame of predictors, where each row
#'  corresponds to an observation and each column to a predictor.
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
#'  a `predict` method and can run the code
#'  `predict(intermediate_estimator, X)`.
#'  If `NULL`, a quantile forest with default arguments is fitted by calling
#'  `grf::quantile_forest(X, Y)` from \pkg{grf} package.
#'  Default is `NULL`.
#'
#'  !!! one of `grf`, (then add `neural_network`, etc.)
#'
#'  !!! ensure `intermediate_estimator` is one of `grf`, etc...
#'
#'  !!! code `predict.extreme_forest(object, newdata = NULL, quantiles = NULL, quantile_intermediate = NULL)`
#'  handle errors for
#'   * `object` that is not `extreme_forest`
#'   * ...
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
#'   For example, `predict(intermediate_estimator, X)`
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
extreme_forest <- function(X, Y, min.node.size = 5, lambda = 0.001,
                           intermediate_estimator = NULL) {

  # validate inputs
  validate_data(X, Y)

  validate_params(min.node.size, lambda)

  validate_intermediate_estimator(intermediate_estimator, X)

  # return extreme_forest object
  validate_extreme_forest(new_extreme_forest(
    X, Y, min.node.size, lambda,
    intermediate_estimator
  ))
}


validate_data <- function(X, Y) {
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


validate_intermediate_estimator <- function(estimator, X) {
  ## intermdiate_estimator numeric_matrix -> intermediate_estimator
  ## returns estimator if it is well formed, throws error if not

  # if not NULL inspect
  if (!is.null(estimator)) {

    # check whether estimator has method predict
    if (!has_method(estimator, predict)) {
      stop("error", call. = FALSE)
    }

    # check whether predict(estimator, X) runs
    tryCatch(
      error = function(cnd) {
        abort_predict_on_fail(
          "intermediate_estimator",
          class(estimator)[1],
          "X",
          cnd$message
        )
      },
      predict(estimator, X)
    )
  }

  invisible(estimator)
}


validate_extreme_forest <- function(ef) {
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
    lambda = lambda
  ),
  class = "extreme_forest"
  )
}


fit_intermediate_quantile <- function(X, Y, intermediate_estimator) {
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


has_method <- function(object, generic) {
  ## object generic -> boolean
  ## produces true if `object` has method `generic`, false otherwise

  ch <- deparse(substitute(generic))

  any(grepl(
    ch,
    sapply(
      class(object),
      function(cl) {
        methods("class" = cl)
      }
    )
  ))
}

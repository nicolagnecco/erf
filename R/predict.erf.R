#' Predict an extremal random forest (ERF)
#'
#'
#' Predicts ... !!! write description
#'
#'
#' !!! write details
#'
#'
#' @param object Fitted "`erf`" object.
#'
#' @param newdata Numeric matrix (!!!) or `data.frame` of test predictor values
#'  at which predictions are to be made. If `NULL`, predictions are made on the
#'  training data instead.
#'  !!! Talk about out-of-bag predictions?
#'  For further information see [grf::quantile_forest()].
#'  Default is `NULL`.
#'
#' @param quantiles Numeric vector of quantile levels at which estimates are
#'  to be made.
#'  Default is `c(0.95, 0.99)`.
#'
#' @param intermediate_quantile Intermediate quantile
#'  level, used to predict the intermediate threshold.
#'  For further information see \insertCite{merg2020;textual}{erf}.
#'
#' @param ... Additional arguments used to predict the intermediate_threshold,
#'  for example, used when evaluating
#'  `predict(object$intermediate_threshold, newdata, ...)` for estimators other
#'  than `quantile_forest` class.
#'
#'
#' @return Numeric matrix with predictions for each test point (rows)
#'  at the desired quantile levels (columns).
#'
#'
#' @examples
#' "!!! add examples"
#'
#'
#' @references
#'  \insertAllCited{}
#'
#'
#' @export
predict.erf <- function(object, newdata = NULL,
                        quantiles = c(0.95, 0.99),
                        intermediate_quantile = 0.8,
                        ...) {

  # validate object
  validate_erf(object)

  # validate newdata
  validate_newdata(newdata, object)

  # validate intermediate_quantile !!! between 0-1, scalar
  # validate quantiles !!! between 0-1, numeric_vector, none less than intermediate_quantile

  # predict intermediate quantile on test data
  Q_x <- predict_intermediate_threshold(
    intermediate_threshold = object$intermediate_threshold,
    newdata = newdata,
    intermediate_quantile = intermediate_quantile,
    ...
  )

  # predict intermediate quantile on training data
  Q_X <- predict_intermediate_threshold(
    intermediate_threshold = object$intermediate_threshold,
    intermediate_quantile = intermediate_quantile,
    ...
  )

  # compute optimal GPD parameters
  gpd_pars <- fit_conditional_gpd(
    object$quantile_forest,
    newdata,
    Q_X,
    lambda = object$lambda
  )

  # predict quantiles
  compute_extreme_quantiles(
    gpd_pars,
    Q_x,
    quantiles,
    intermediate_quantile
  )
}

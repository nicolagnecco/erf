#' Predict an extremal random forest (ERF)
#'
#'
#' Makes prediction using a fitted `erf` object.
#'
#'
#' @param object Fitted `erf` object.
#'
#' @param newdata Numeric matrix or `data.frame` of test predictor values
#'  at which predictions are to be made. If `NULL`, predictions are made on the
#'  training data instead.
#'  For further information see [grf::quantile_forest()].
#'  Default is `NULL`.
#'
#' @param quantiles Numeric vector of quantile levels at which estimates are
#'  to be made.
#'  Default is `c(0.95, 0.99)`.
#'
#' @param ... Dots. Currently ignored.
#'
#'
#' @return Numeric matrix with predictions for each test point (rows)
#'  at the desired quantile levels (columns).
#'
#'
#'
#' @export
predict.erf <- function(object,
                        newdata = NULL,
                        quantiles = c(0.95, 0.99),
                        ...) {

  # validate object
  validate_erf(object)

  # validate newdata
  validate_newdata_X(newdata, object)

  # validate intermediate_quantile !!! between 0-1, scalar
  # validate quantiles !!! between 0-1, numeric_vector, none less than intermediate_quantile

  # predict intermediate quantile
  # !!! if newdata is not null
  Q_x <- predict_intermediate_quantile(
    intermediate_threshold = object$intermediate_threshold,
    newdata = newdata,
    intermediate_quantile = object$intermediate_quantile
  )

  # compute optimal GPD parameters
  gpd_pars <- predict_gpd_params(
    object,
    newdata
  )

  # predict quantiles
  predict_extreme_quantiles(
    gpd_pars,
    Q_x,
    quantiles,
    object$intermediate_quantile
  )
}

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
#'  training data insetead.
#'  !!! Talk about out-of-bag predictions?
#'  For further information see [grf::quantile_forest()].
#'  Default is `NULL`.
#'
#' @param quantiles Numeric vector of quantile levels at which estimates are
#'  to be made.
#'  Default is `c(0.95, 0.99)`.
#'
#' @param quantile_intermediate Intermediate quantile
#'  level, used to predict the intermediate threshold.
#'  For further information see \insertCite{merg2020;textual}{erf}.
#'
#' @param ... Additional arguments passed to or from other methods.
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
                        quantile_intermediate = 0.8,
                        ...) {

  # validate object
  validate_erf(object)

  # validate newdata
  validate_newdata(newdata, object)

  # validate quantile_intermediate !!! between 0-1, scalar
  # validate quantiles !!! between 0-1, numeric_vector, none less than quantile_intermediate

  # predict intermediate quantile on test data
  Q_x <- predict_intermediate_threshold(
    object$intermediate_threshold,
    newdata,
    quantile_intermediate,
    ...
  )

  # compute optimal GPD parameters
  gpd_pars <- fit_conditional_gpd(
    object,
    newdata,
    quantile_intermediate
  )

  # predict quantiles
  compute_extreme_quantiles(
    gpd_pars,
    Q_x,
    quantiles,
    quantile_intermediate
  )
}

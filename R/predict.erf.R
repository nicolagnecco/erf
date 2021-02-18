#' !!! code `predict.erf(object, newdata = NULL, quantiles = NULL, quantile_intermediate = NULL)`
#'  handle errors for
#'   - `object` that is not `erf`
#'   - `...`
#'   @export
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

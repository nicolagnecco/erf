#' !!! code `predict.extreme_forest(object, newdata = NULL, quantiles = NULL, quantile_intermediate = NULL)`
#'  handle errors for
#'   - `object` that is not `extreme_forest`
#'   - `...`
#'   @export
predict.extreme_forest <- function(object, newdata = NULL,
                                   quantiles = c(0.95, 0.99),
                                   quantile_intermediate = 0.8,
                                   ...) {

  # validate object
  validate_extreme_forest(object)

  # validate newdata
  validate_newdata(newdata, object)

  # predict intermediate quantile
  t_x0 <- predict_intermediate_quantile(
    object$intermediate_threshold,
    newdata,
    quantile_intermediate,
    ...
  )

  # compute optimal GPD parameters
  gpd_pars <- fit_conditional_gpd(object, newdata) # !!! recode this

  # predict quantiles
  compute_extreme_quantiles(gpd_pars, t_x0, quantiles, quantile_intermediate) # !!! recode this

}

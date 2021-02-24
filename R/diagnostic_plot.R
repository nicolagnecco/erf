diagnostic_plot <- function(object,
                            newdata_X = NULL,
                            newdata_Y = NULL){

  # validate object
  validate_erf(object)

  # validate newdata
  validate_newdata(newdata_X, object)
  # and newdata_Y

  # validate intermediate_quantile !!! between 0-1, scalar
  # validate quantiles !!! between 0-1, numeric_vector, none less than intermediate_quantile

  # predict intermediate quantile
  # !!! if newdata is not null
  Q_x <- predict_intermediate_quantile(
    intermediate_threshold = object$intermediate_threshold,
    newdata = newdata_X,
    intermediate_quantile = object$intermediate_quantile
  )

  # compute optimal GPD parameters
  gpd_pars <- predict_gpd_params(
    object,
    newdata_X
  )

  # compute model assessment
  compute_model_assessment(gpd_params, newdata_Y, Q_x)

}

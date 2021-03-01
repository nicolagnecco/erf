compute_model_assessment <- function(object,
                            newdata_X = NULL,
                            newdata_Y = NULL){
  ## erf numeric_matrix numeric_vector -> tibble
  ## produce a tibble with columns `observed_quantiles` and `theoretical_quantiles`

  # validate object
  validate_erf(object)

  # validate newdata_X and newdata_Y
  lst <- validate_newdata_X_and_Y(newdata_X, newdata_Y, object)
  newdata_X <- lst$newdata_X
  newdata_Y <- lst$newdata_Y

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
  compute_model_assessment_helper(gpd_pars, newdata_Y, Q_x)

}

compute_model_assessment_helper <- function(gpd_pars, Y, Q) {
  ## tibble numeric_vector numeric_vector -> tibble
  ## produce a tibble with columns `observed_quantiles` and `theoretical_quantiles`

  exc_ind <- which(Y > Q)
  Z <- (Y - Q)[exc_ind]
  sigma <- gpd_pars[[1]][exc_ind]
  xi <- gpd_pars[[2]][exc_ind]
  pseudo_obs <- compute_pseudo_observations(Z, sigma, xi)
  n <- length(pseudo_obs)

  tibble::tibble(
    "observed_quantiles" = sort(pseudo_obs),
    "theoretical_quantiles" = stats::qexp(seq_len(n) / (n + 1))
  )
}

compute_pseudo_observations <- function(exc_data, sigma, xi) {
  ## numeric_vector numeric numeric -> numeric_vector
  ## compute pseudo observations on exponential margin

  tmp_res <- 1 + xi * exc_data / sigma
  idx <- which(tmp_res <= 0)

  if (length(idx) >= 1) {
    warning_obs_outside_support(idx)
  }

  pseudo_obs <- 1 / xi[tmp_res > 0] * log(tmp_res[tmp_res > 0])
  return(pseudo_obs)
}

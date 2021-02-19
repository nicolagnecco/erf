fit_conditional_gpd <- function(quantile_forest, newdata, Q_X, lambda) {
  ## quantile_forest numeric_matrix|NULL numeric_matrix numeric -> tibble
  ## produce a tibble with MLE GPD scale and shape parameter for each test point
  ## each row corresponds to a test point, each column to a GPD parameter,
  ## namely, `scale` and `shape`

  # check size for similarity weights

  # extract response vector
  Y <- quantile_forest$Y.orig

  # compute similarity weights
  W <- as.matrix(grf::get_sample_weights(
    forest = quantile_forest,
    newdata = newdata
  ))

  # compute optimal GPD parameters
  fit_conditional_gpd_helper(W, Y, Q_X, lambda)

}


fit_conditional_gpd_helper <- function(W, Y, Q, lambda) {
  ## numeric_matrix numeric_vector numeric_vector numeric -> tibble
  ## produce a tibble with MLE GPD scale and shape parameter for each test point
  ## each row corresponds to a test point, each column to a GPD parameter,
  ## namely, `scale` and `shape`

  # compute exceedances
  exc_ind <- which(Y > Q)
  Z <- (Y - Q)[exc_ind]
  W <- W[, exc_ind]
  ntest <- nrow(W)

  # initial guess for GPD parameters
  init_pars <- ismev::gpd.fit(Z, 0, show = FALSE)$mle

  # GPD parameters for each test observation
  purrr::map_dfr(seq_len(ntest), function(i) {
    wi_x <- W[i, ]
    optim_wrapper(wi_x, init_pars, Z, lambda, init_pars[2])
  })
}

optim_wrapper <- function(wi_x, init_pars, Z, lambda, xi_prior) {
  ## numeric_vector numeric_vector function numeric_vector numeric numeric
  ## -> numeric_vector
  ## return optimal scale and shape parameters of the weighted log-likelihood
  res <- stats::optim(
    par = init_pars,
    fn = weighted_llh,
    data = Z,
    weights = wi_x,
    lambda = lambda,
    xi_prior = xi_prior
  )$par
  names(res) <- c("sigma", "xi")
  return(res)
}


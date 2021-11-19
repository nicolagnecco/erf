predict_gpd_params <- function(object, newdata) {
  ## erf|erf_lw numeric_matrix|NULL -> tibble
  ## produce a tibble with MLE GPD scale (sigma) and shape (xi) parameter
  ## for each test point;
  ## each row corresponds to a test point, each column to a GPD parameter,
  ## namely, `sigma` and `xi`

  # check size for similarity weights
  # !!!

  # extract response and intermediate quantile vector
  Y <- object$quantile_forest$Y.orig
  Q_X <- object$Q_X

  # compute similarity weights
  W <- as.matrix(grf::get_forest_weights(
    forest = object$quantile_forest,
    newdata = newdata
  ))

  # compute optimal GPD parameters
  predict_gpd_params_helper(W, Y, Q_X, object$lambda)

}


predict_gpd_params_helper <- function(W, Y, Q, lambda) {
  ## numeric_matrix numeric_vector numeric_vector numeric -> tibble
  ## produce a tibble with MLE GPD scale (sigma) and shape (xi) parameter
  ## for each test point;
  ## each row corresponds to a test point, each column to a GPD parameter,
  ## namely, `sigma` and `xi`

  # compute exceedances
  exc_ind <- which(Y > Q)
  Z <- (Y - Q)[exc_ind]
  W <- W[, exc_ind, drop = FALSE]
  ntest <- nrow(W)

  # initial guess for GPD parameters
  init_pars <- ismev::gpd.fit(Z, 0, show = FALSE)$mle

  # GPD parameters for each test observation
  tibble::tibble(
    "sigma" = numeric(0),
    "xi" = numeric(0)
  ) %>%
    dplyr::bind_rows(
      purrr::map_dfr(seq_len(ntest), function(i) {
        wi_x <- W[i, ]
        optim_wrapper(wi_x, init_pars, Z, lambda, init_pars[2])
      })
    )

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


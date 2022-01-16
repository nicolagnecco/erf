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
  tau_0 <- object$intermediate_quantile

  # compute similarity weights
  W <- as.matrix(grf::get_forest_weights(
    forest = object$quantile_forest,
    newdata = newdata
  ))

  # compute optimal GPD parameters
  predict_gpd_params_helper(W, Y, Q_X, object$lambda, tau_0)

}


predict_gpd_params_helper <- function(W, Y, Q, lambda, intermediate_quantile) {
  ## numeric_matrix numeric_vector numeric_vector numeric numeric(0, 1)-> tibble
  ## produce a tibble with MLE GPD scale (sigma) and shape (xi) parameter
  ## for each test point;
  ## each row corresponds to a test point, each column to a GPD parameter,
  ## namely, `sigma` and `xi`

  # compute exceedances
  exc_ind <- which(Y > Q)
  Z <- (Y - Q)[exc_ind]
  W <- W[, exc_ind, drop = FALSE]

  # initial guess for GPD parameters
  init_pars <- ismev::gpd.fit(Z, 0, show = FALSE)$mle

  # GPD parameters for each test observation
  predict_gpd_params_cpp(
    init_pars = init_pars, data = Z, weights_mat = t(W),
    lambda = lambda, xi_prior = init_pars[2],
    intermediate_quantile = intermediate_quantile
  ) %>%
    dplyr::as_tibble(.name_repair = ~c("sigma", "xi"))

}

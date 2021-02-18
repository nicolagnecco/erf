fit_conditional_gpd <- function(erf, newdata) {
  ## erf numeric_matrix -> numeric_matrix
  ## produce matrix with MLE GPD scale and shape parameter for each test point
  ## each row corresponds to a test point, each column to a GPD parameter,
  ## namely, scale and shape

  # check size for similarity weights

  # compute similarity weights

  # compute intermediate quantile

  # fit gpd parameters
  fit_conditional_gpd_helper(Wi_x, Y, Q_X, lambda)

}


fit_conditional_gpd_helper <- function(W, Y, Q, lambda) {
  ## numeric_matrix numeric_vector numeric_vector numeric -> numeric_matrix
  ## produce matrix with MLE GPD scale and shape parameter for each test point
  ## each row corresponds to a test point, each column to a GPD parameter,
  ## namely, scale and shape

  ntest <- nrow(wi_x0)
  Y <- object$Y.orig
  exc_idx <- which(Y - t_xi > 0)
  exc_data <- (Y - t_xi)[exc_idx]
  init_par <- ismev::gpd.fit(exc_data, 0, show = FALSE)$mle

  wi_x0 <- wi_x0[, exc_idx]
  EVT_par <- purrr::map_dfr(
    1:ntest, optim_wrap, init_par, weighted_llh,
    exc_data, wi_x0, lambda, init_par[2]
  )

  return(as.matrix(EVT_par))

}





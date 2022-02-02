predict_extreme_quantiles <- function(gpd_pars,
                                      Q_x,
                                      quantiles,
                                      intermediate_quantile) {
  ## tibble numeric_vector numeric_vector(0, 1) numeric(0, 1) -> numeric_matrix
  ## produce matrix with estimated extremes quantiles. The value at (i, j) gives
  ## the estimated quantiles[j] for test sample i

  res <- matrix(nrow = length(Q_x), ncol = length(quantiles))

  for (j in seq_along(quantiles)) {
    res[, j] <- q_GPD(
      p = quantiles[j], p0 = intermediate_quantile, t_x0 = Q_x,
      sigma = gpd_pars[[1]], xi = gpd_pars[[2]]
    )
  }

  colnames(res) <- paste("quantile = ", quantiles)

  return(res)
}

q_GPD <- function(p, p0, t_x0, sigma, xi) {
  ## numeric(0, 1) numeric(0, 1) numeric_vector numeric_vector
  ## numeric_vector -> numeric_vector
  ## estimates extreme quantiles of GPD

  (((1 - p) / (1 - p0))^
     {
       -xi
     } - 1) * (sigma / xi) + t_x0
}

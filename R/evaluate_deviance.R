
evaluate_deviance <- function(gpd_pars, Y, Q_X) {
  ## tibble numeric_vector (2x) -> numeric
  ## evaluate the GDP deviance
  ## ASSUME: nrow(gpd_pars) = length(Y) = length(Q_X) != 0

  Z <- Y - Q_X
  data <- Z[Z > 0]
  sigma <- gpd_pars[[1]][Z > 0]
  xi <- gpd_pars[[2]][Z > 0]
  y <- 1 + (xi / sigma) * data

  if (min(sigma) <= 0) {
    return(10^6)
  } else if (min(y) <= 0) {
    return(10^6)
  } else {
    return(sum(log(sigma) + (1 + 1 / xi) * log(y)))
  }

}

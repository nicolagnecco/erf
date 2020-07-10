square_loss <- function(y, y_hat){
  ## numeric_vector numeric_vector -> numeric
  ## produce square loss
  (y - y_hat) ^ 2
}

quantile_loss <- function(y, y_hat, alpha){
  ## numeric_vector numeric_vector numeric(0, 1) -> numeric
  ## produce quantile loss
  if (y > y_hat){
    alpha * abs(y - y_hat)
  } else {
    (1 - alpha) * abs(y - y_hat)
  }
}


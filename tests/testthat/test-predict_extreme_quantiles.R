test_that("predict_extreme_quantiles works", {

  Q_x <- predict_intermediate_quantile(
    intermediate_threshold = erf_1$intermediate_threshold,
    newdata = X_test,
    intermediate_quantile = intermediate_quantile
  )

  Q_X <- predict_intermediate_quantile(
    intermediate_threshold = erf_1$intermediate_threshold,
    intermediate_quantile = intermediate_quantile
  )

  gpd_pars <- predict_gpd_params(
    erf_1,
    newdata = X_test
  )

  my_extreme_quantile <- function() {
    res <- matrix(nrow = length(Q_x), ncol = length(quantiles))
    for (j in seq_along(quantiles)) {
      res[, j] <- q_GPD(
        p = quantiles[j], p0 = intermediate_quantile, t_x0 = Q_x,
        sigma = gpd_pars$sigma, xi = gpd_pars$xi
      )
    }
    res
  }

  expect_equivalent(
    predict_extreme_quantiles(
      gpd_pars,
      Q_x,
      quantiles,
      intermediate_quantile),
    my_extreme_quantile()
  )
})

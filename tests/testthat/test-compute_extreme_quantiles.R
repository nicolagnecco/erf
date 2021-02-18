test_that("compute_extreme_quantiles works", {

  Q_x <- predict_intermediate_threshold(
    intermediate_threshold = erf_1$intermediate_threshold,
    newdata = X_test,
    quantile_intermediate = quantile_intermediate
  )

  gpd_pars <- fit_conditional_gpd(
    erf_1,
    newdata = X_test,
    quantile_intermediate = quantile_intermediate
  )

  my_extreme_quantile <- function() {
    res <- matrix(nrow = length(Q_x), ncol = length(quantiles))
    for (j in seq_along(quantiles)) {
      res[, j] <- q_GPD(
        p = quantiles[j], p0 = quantile_intermediate, t_x0 = Q_x,
        sigma = gpd_pars$sigma, xi = gpd_pars$xi
      )
    }
    res
  }

  expect_equivalent(
    compute_extreme_quantiles(
      gpd_pars,
      Q_x,
      quantiles,
      quantile_intermediate),
    my_extreme_quantile()
  )
})

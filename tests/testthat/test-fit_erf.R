test_that("fit_erf works", {

  expect_equal({
    set.seed(42)
    fit_erf(X_small, Y_small,
            min.node.size, lambda, "grf",
            intermediate_quantile = intermediate_quantile)
    },
    {

      set.seed(42)
      intermediate_threshold_5 <- grf::quantile_forest(X_small, Y_small)
      quant_forest <- grf::quantile_forest(X_small, Y_small,
                                           min.node.size = min.node.size)
      Q_X <- predict_intermediate_quantile(intermediate_threshold_5,
                     intermediate_quantile = intermediate_quantile)
      structure(list(
        "quantile_forest" = quant_forest,
        "min.node.size" = min.node.size,
        "lambda" = lambda,
        "intermediate_threshold" = intermediate_threshold_5,
        "intermediate_quantile" = intermediate_quantile,
        "Q_X" = Q_X
  ),
  class = "erf")
  })

  cnd <- rlang::catch_cnd(fit_erf(X, Y,
                                  min.node.size, lambda,
                                  "foo_estimator", intermediate_quantile))

  expect_s3_class(cnd, "error_not_implemented")

})

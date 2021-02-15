test_that("predict_intermediate_quantile works", {

  # intermediate threshold is `quantile_forest`
  expect_equal(
    predict_intermediate_quantile(
      intermediate_threshold = intermediate_threshold_2,
      newdata = X_test_small,
      quantile_intermediate = 0.8
    ),
    grf:::predict.quantile_forest(intermediate_threshold_2, X_test_small, quantiles = .8)
  )

  # intermediate threshold is another S3 class
  expect_equal(
    predict_intermediate_quantile(
      intermediate_threshold = intermediate_threshold_4,
      newdata = data.frame(X_test_small),
      quantile_intermediate = 0.8
    ),
    predict(intermediate_threshold_4, newdata = data.frame(X_test_small))
  )
})

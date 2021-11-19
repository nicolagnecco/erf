test_that("predict_intermediate_quantile works", {

  # intermediate threshold is `quantile_forest`
  expect_equal(
    predict_intermediate_quantile(
      intermediate_threshold = intermediate_threshold_2,
      newdata = X_test_small,
      intermediate_quantile = 0.8
    ),
    unlist(
      grf:::predict.quantile_forest(intermediate_threshold_2, X_test_small,
                                    quantiles = .8)
    )
  )

  expect_equal(
    predict_intermediate_quantile(
      intermediate_threshold = intermediate_threshold_1,
      newdata = NULL,
      intermediate_quantile = 0.8
    ),
    unlist(
      grf:::predict.quantile_forest(intermediate_threshold_1, quantiles = .8)
    )
  )

  expect_equal(
    predict_intermediate_quantile(
      intermediate_threshold = intermediate_threshold_2,
      intermediate_quantile = 0.8
    ),
    unlist(
      grf:::predict.quantile_forest(intermediate_threshold_2, quantiles = .8)
    )
  )

  # intermediate threshold is another S3 class
  cnd <- rlang::catch_cnd(
    predict_intermediate_quantile(
      intermediate_threshold = intermediate_threshold_4,
      newdata = data.frame(X_test_small),
      intermediate_quantile = 0.8
    )
  )
  expect_s3_class(cnd, "error_not_implemented")
})

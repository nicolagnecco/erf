test_that("fit_intermediate_threshold works", {
  expect_equal(
    {
      set.seed(42)
      fit_intermediate_threshold(X_small, Y_small, "grf")
    },
    {
      set.seed(42)
      grf::quantile_forest(X_small, Y_small)
    }
  )

  expect_equal(
    {
      set.seed(42)
      fit_intermediate_threshold(X_small, Y_small, "grf", min.node.size = 10)
    },
    {
      set.seed(42)
      grf::quantile_forest(X_small, Y_small, min.node.size = 10)
    }
  )


  cnd <- rlang::catch_cnd(fit_intermediate_threshold(X_small, Y_small, "neural_nets"))
  expect_s3_class(cnd, "error_not_implemented")

  cnd <- rlang::catch_cnd(fit_intermediate_threshold(X_small, Y_small, "foo"))
  expect_s3_class(cnd, "error_not_implemented")
})

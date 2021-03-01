test_that("compute_model_assessment works", {

  res <- compute_model_assessment(erf_1, newdata_X = NULL, newdata_Y = NULL)
  expect_s3_class(res, "tbl_df")


  res <- compute_model_assessment(erf_1, newdata_X = X_test, newdata_Y = Y_test)
  expect_s3_class(res, "tbl_df")


  cnd <- rlang::catch_cnd(
    compute_model_assessment(erf_1, newdata_X = X_test[, 1:3],
                             newdata_Y = Y_test)
  )
  expect_s3_class(cnd, "error_wrong_columns")


  cnd <- rlang::catch_cnd(
    compute_model_assessment(erf_1, newdata_X = X_test[, 1],
                             newdata_Y = Y_test)
  )
  expect_s3_class(cnd, "error_wrong_dimension")


  cnd <- rlang::catch_cnd(
    compute_model_assessment(erf_1, newdata_X = X_test[integer(0), ],
                             newdata_Y = Y_test)
  )
  expect_s3_class(cnd, "error_zero_rows")


  cnd <- rlang::catch_cnd(
    compute_model_assessment(erf_1, newdata_X = X_test,
                             newdata_Y = cbind(Y_test, Y_test))
  )
  expect_s3_class(cnd, "error_wrong_dimension")


  cnd <- rlang::catch_cnd(
    compute_model_assessment(erf_1, newdata_X = X_test,
                             newdata_Y = Y_test[integer(0)])
  )
  expect_s3_class(cnd, "error_zero_rows")


  cnd <- rlang::catch_cnd(
    compute_model_assessment(erf_1, newdata_X = X_test,
                             newdata_Y = Y_test[1:3])
  )
  expect_s3_class(cnd, "error_different_n_observations")


  cnd <- rlang::catch_cnd(
    compute_model_assessment(erf_1, newdata_X = X_test[1:10, ],
                             newdata_Y = Y_test)
  )
  expect_s3_class(cnd, "error_different_n_observations")


  cnd <- rlang::catch_cnd(
    compute_model_assessment(erf_1, newdata_X = NULL,
                             newdata_Y = Y_test)
  )
  expect_s3_class(cnd, "error_both_null")


  cnd <- rlang::catch_cnd(
    compute_model_assessment(erf_1, newdata_X = X_test,
                             newdata_Y = NULL)
  )
  expect_s3_class(cnd, "error_both_null")

})


test_that("compute_model_assessment_helper works", {

  gpd_pars <- tibble::tibble(
    sigma = numeric(0), xi = numeric(0)
  )
  Y <- numeric(0)
  Q <- numeric(0)
  expect_equal(compute_model_assessment_helper(gpd_pars, Y, Q),
               tibble::tibble("observed_quantiles" = numeric(0),
                              "theoretical_quantiles" = numeric(0))
  )


  gpd_pars <- tibble::tibble(
    sigma = c(0.001, 0.001), xi = c(-1, -1)
  )

  Y <- c(10, 10)
  Q <- c(0, 0)

  cnd <- rlang::catch_cnd(compute_model_assessment_helper(gpd_pars, Y, Q))
  expect_s3_class(cnd, "warn_outside_support")
  expect_warning(res <- compute_model_assessment_helper(gpd_pars, Y, Q))
  expect_equal(res,
               tibble::tibble("observed_quantiles" = numeric(0),
                              "theoretical_quantiles" = numeric(0))
  )


  gpd_pars <- tibble::tibble(
    sigma = c(1, 0.001), xi = c(1, -1)
  )

  Y <- c(10, 10)
  Q <- c(0, 0)

  cnd <- rlang::catch_cnd(compute_model_assessment_helper(gpd_pars, Y, Q))
  expect_s3_class(cnd, "warn_outside_support")
})

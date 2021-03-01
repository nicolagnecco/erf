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

test_that("validate_intermediate_estimator works", {

  # estimator is abbreviated
  expect_equal(validate_intermediate_estimator("gr"), "grf")
  expect_equal(validate_intermediate_estimator("neu"), "neural_nets")


  # estimator is exact
  expect_equal(validate_intermediate_estimator("grf"), "grf")
  expect_equal(validate_intermediate_estimator("neural_nets"), "neural_nets")
  expect_equal(validate_intermediate_estimator(c("grf", "neural_nets")), "grf")


  # estimator is wrong
  expect_error(validate_intermediate_estimator("ggg"))
  expect_error(validate_intermediate_estimator("neural_netttt"))

  # estimator is vector
  expect_error(validate_intermediate_estimator(c("ggg", "neural_netttt")))
})


test_that("validata_newdata works", {
  expect_equal(validate_newdata_X(NULL, erf_2), NULL)

  expect_equal(validate_newdata_X(X_test_small, erf_2),
                X_test_small)

  cnd <- rlang::catch_cnd(validate_newdata_X(X_test_small[, 1], erf_2))
  expect_s3_class(cnd, "error_wrong_dimension")

  cnd <- rlang::catch_cnd(validate_newdata_X(X_test, erf_2))
  expect_s3_class(cnd, "error_wrong_columns")

  cnd <- rlang::catch_cnd(validate_newdata_X(X_test[integer(0), ], erf_1))
  expect_s3_class(cnd, "error_zero_rows")


})


test_that("has_method works", {
  # S3 object with method
  expect_true(has_method(intermediate_threshold_1, predict))
  expect_true(has_method(structure(list(), class = "lm"), coef))

  # S3 object with no method
  expect_false(has_method(structure(list(), class = "foo"), predict))

  # base object
  expect_false(has_method(2, predict))
})

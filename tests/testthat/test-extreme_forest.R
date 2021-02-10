test_that("validate_intermediate_estimator works", {

  # estimator is NULL
  expect_equal(validate_intermediate_estimator(NULL, X), NULL)

  # estimator does not have predict method
  expect_error(validate_intermediate_estimator(intermediate_estimator_2, X))

  # estimator has predict method but predict call does not work
  expect_error(validate_intermediate_estimator(intermediate_estimator_1,
                                               X[, -1]))
  expect_error(validate_intermediate_estimator(intermediate_estimator_3,
                                               X[, -1]))
  expect_error(validate_intermediate_estimator(intermediate_estimator_4,
                                               X[, -1]))
  expect_error(validate_intermediate_estimator(intermediate_estimator_4,
                                               X))

  # estimator has correct class and predict works
  expect_equal(validate_intermediate_estimator(intermediate_estimator_1, X),
               intermediate_estimator_1)
  expect_equal(validate_intermediate_estimator(intermediate_estimator_4,
                                               as.data.frame(X)),
               intermediate_estimator_4)

})

test_that("has_method works", {
  # S3 object with method
  expect_true(has_method(intermediate_estimator_1, predict))
  expect_true(has_method(intermediate_estimator_4, coef))

  # S3 object with no method
  expect_false(has_method(intermediate_estimator_2, predict))

  # base object
  expect_false(has_method(2, predict))
})

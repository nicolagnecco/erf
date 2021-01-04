test_that("validate_fn works", {
  expect_equal(validate_fn(lm), TRUE)
  expect_error(validate_fn(2), "2 is not a function.")

})

test_that("validate_X works", {
  X <- matrix(0, nrow = 3, ncol = 2)
  expect_equal(validate_X(X), TRUE)

  X <- matrix(nrow = 3, ncol = 2)
  expect_error(validate_X(X), "The predictor X cannot contain NAs.")

  X <- matrix("a", nrow = 3, ncol = 2)
  expect_error(validate_X(X), "The predictor X must be a numeric matrix.")

  X <- numeric(length = 10)
  expect_error(validate_X(X), "The predictor X must be a numeric matrix.")

  X <- character(length = 10)
  expect_error(validate_X(X), "The predictor X must be a numeric matrix.")


})



test_that("validate_fn works", {
  expect_equal(validate_fn(lm), TRUE)
  expect_error(validate_fn(2), "2 is not a function.")

})



test_that("abort_predict_on_fail works", {
  err <- rlang::catch_cnd(
    abort_predict_on_fail("int_est", "this.class", "my_X", "you screwed it")
  )
  expect_s3_class(err, "error_predict_fails")
})

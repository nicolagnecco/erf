test_that("new_erf works", {

  expect_equal({
  set.seed(42)
  new_erf(X_small, Y_small,
                     min.node.size, lambda, intermediate_threshold_2)},
  {

  set.seed(42)
  quant_forest <- grf::quantile_forest(X_small, Y_small,
                                       min.node.size = min.node.size)
  structure(list(
    "quantile_forest" = quant_forest,
    "min.node.size" = min.node.size,
    "lambda" = lambda,
    "intermediate_threshold" = intermediate_threshold_2
  ),
  class = "erf")
  })

})

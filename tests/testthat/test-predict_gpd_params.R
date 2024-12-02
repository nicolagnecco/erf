test_that("predict_gpd_params works", {

  # compare to helper function for large dataset
  Q <- predict_intermediate_quantile(
    erf_1$intermediate_threshold,
    intermediate_quantile = intermediate_quantile
  )

  expect_equal(
    {
      predict_gpd_params(
        object = erf_1,
        newdata = X_test
      )
    },
    {

      W <- as.matrix(
        grf::get_forest_weights(
          forest = erf_1$quantile_forest,
          newdata = X_test)
      )
      predict_gpd_params_helper(W, Y, Q, erf_1$lambda)
    })

  # compare to helper function for small dataset
  Q_small <- predict_intermediate_quantile(
    erf_2$intermediate_threshold,
    intermediate_quantile = intermediate_quantile
  )

  expect_equal(
    {
      predict_gpd_params(
        object = erf_2,
        newdata = NULL
      )
    },
    {

      W_small <- as.matrix(
        grf::get_forest_weights(
          forest = erf_2$quantile_forest)
      )
      predict_gpd_params_helper(W_small, Y_small, Q_small, erf_2$lambda)
    })

})


test_that("predict_gpd_params_helper works", {
  Q <- predict_intermediate_quantile(erf_1$intermediate_threshold,
                                     intermediate_quantile = 0.8)

  W <- as.matrix(grf::get_forest_weights(erf_1$quantile_forest))

  expect_s3_class(predict_gpd_params_helper(W, Y, Q, 0.01), "tbl_df")
  expect_equal(dim(predict_gpd_params_helper(W, Y, Q, 0.01)), c(nrow(W), 2))

  W <- as.matrix(grf::get_forest_weights(
    forest = erf_1$quantile_forest,
    newdata = X_test
  ))

  expect_equal(dim(predict_gpd_params_helper(W, Y, Q, 0)), c(nrow(W), 2))

  # What happens if there are no test observations?
  W <- W[integer(0), ]
  gpd_params <- predict_gpd_params_helper(W, Y, Q, 0)
  expect_equal(dim(gpd_params), c(nrow(W), 2))
  expect_equal(gpd_params,
               tibble::tibble("sigma" = numeric(0), "xi" = numeric(0)))

})

test_that("optim_wrapper works", {

  Q <- predict_intermediate_quantile(intermediate_threshold_1,
                                     intermediate_quantile = 0.8)
  exc_ind <- which(Y > Q)
  Z <- (Y - Q)[exc_ind]
  W <- as.matrix(grf::get_forest_weights(intermediate_threshold_1))[, exc_ind]

  ntest <- nrow(W)

  init_pars <- ismev::gpd.fit(Z, 0, show = FALSE)$mle

  expect_lt(
    sum(optim_wrapper(rep(1, length = length(Z)),
                      init_pars, Z, lambda = 1e4,
                      init_pars[2])[2] -init_pars[2]), 1e-4)
})



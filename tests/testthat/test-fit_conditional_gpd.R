test_that("fit_conditional_gpd works", {

  # compare to the deprecated version for large dataset
  Q <- predict_intermediate_threshold(
    erf_1$intermediate_threshold,
    intermediate_quantile = 0.8
  )

  expect_equal(
    {
      fit_conditional_gpd(
        quantile_forest = erf_1$quantile_forest,
        newdata = X_test,
        Q_X = Q,
        lambda = erf_1$lambda
      )
    },
    {

      W <- as.matrix(
        grf::get_sample_weights(
          forest = erf_1$quantile_forest,
          newdata = X_test)
      )
      fit_conditional_gpd_helper(W, Y, Q, erf_1$lambda)
    })

  # compare to the deprecated version for small dataset
  Q_small <- predict_intermediate_threshold(
    erf_2$intermediate_threshold,
    intermediate_quantile = 0.8
  )

  expect_equal(
    {
      fit_conditional_gpd(
        quantile_forest = erf_2$quantile_forest,
        newdata = NULL,
        Q_X = Q_small,
        lambda = erf_2$lambda
      )
    },
    {

      W_small <- as.matrix(
        grf::get_sample_weights(
          forest = erf_2$quantile_forest)
      )
      fit_conditional_gpd_helper(W_small, Y_small, Q_small, erf_2$lambda)
    })

})


test_that("fit_conditional_gpd_helper works", {
  ## numeric_matrix numeric_vector numeric_vector numeric -> tibble
  ## produce a tibble with MLE GPD scale and shape parameter for each test point
  ## each row corresponds to a test point, each column to a GPD parameter,
  ## namely, `scale` and `shape

  Q <- predict_intermediate_threshold(erf_1$intermediate_threshold,
                                     intermediate_quantile = 0.8)

  W <- as.matrix(grf::get_sample_weights(erf_1$quantile_forest))

  expect_s3_class(fit_conditional_gpd_helper(W, Y, Q, 0.01), "tbl_df")
  expect_equal(dim(fit_conditional_gpd_helper(W, Y, Q, 0.01)), c(nrow(W), 2))

  W <- as.matrix(grf::get_sample_weights(
    forest = erf_1$quantile_forest,
    newdata = X_test
  ))

  expect_equal(dim(fit_conditional_gpd_helper(W, Y, Q, 0)), c(nrow(W), 2))

  W <- W[integer(0), ]
  expect_equal(dim(fit_conditional_gpd_helper(W, Y, Q, 0)), c(nrow(W), 0))

})

test_that("optim_wrapper works", {

  Q <- predict_intermediate_threshold(intermediate_threshold_1,
                                     intermediate_quantile = 0.8)
  exc_ind <- which(Y > Q)
  Z <- (Y - Q)[exc_ind]
  W <- as.matrix(grf::get_sample_weights(intermediate_threshold_1))[, exc_ind]

  ntest <- nrow(W)

  init_pars <- ismev::gpd.fit(Z, 0, show = FALSE)$mle

  expect_lt(
    sum(optim_wrapper(rep(1, length = length(Z)),
                      init_pars, Z, lambda = 1e4,
                      init_pars[2])[2] -init_pars[2]), 1e-4)
})



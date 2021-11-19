test_that("predict.erf works", {
  # !!!
  # cnd <- rlang::catch_cnd(predict.erf(object = erf_1, quantiles = 0.1))
  #
  # expect_s3_class(cnd, "error_quantile_too_low")

  # Check that predict.erf works when newdata is supplied
  Q_x <- predict_intermediate_quantile(
    erf_1$intermediate_threshold,
    newdata = X_test,
    intermediate_quantile = erf_1$intermediate_quantile
  )

  expect_equal({
    predict.erf(
      object = erf_1,
      newdata = X_test,
      quantiles = quantiles)
  },
  {
    predict_erf_internal(
      object = erf_1$quantile_forest,
      quantiles =  quantiles,
      threshold = erf_1$intermediate_quantile,
      newdata = X_test,
      t_xi = erf_1$Q_X,
      t_x0 = Q_x,
      lambda = erf_1$lambda)$predictions
  }
  )

  # Check that predict.erf works when newdata is NULL
  W <- as.matrix(grf::get_forest_weights(erf_1$quantile_forest))

  expect_equal(
    predict.erf(
      object = erf_1,
      quantiles = quantiles),
    predict_erf_internal(
      object = erf_1$quantile_forest,
      quantiles =  quantiles,
      threshold = erf_1$intermediate_quantile,
      wi_x0 = W,
      t_xi = erf_1$Q_X,
      t_x0 = erf_1$Q_X,
      lambda = erf_1$lambda)$predictions
  )

  # Check that predict.erf works with another dataset
  Q_x <- predict_intermediate_quantile(
    erf_2$intermediate_threshold,
    newdata = X_test_small,
    intermediate_quantile = intermediate_quantile
  )

  expect_equal(
    predict.erf(
      object = erf_2,
      newdata = X_test_small,
      quantiles = quantiles),
    predict_erf_internal(
      object = erf_2$quantile_forest,
      quantiles =  quantiles,
      threshold = erf_2$intermediate_quantile,
      newdata = X_test_small,
      t_xi = erf_2$Q_X,
      t_x0 = Q_x,
      lambda = erf_2$lambda)$predictions
  )

  # Check that predict.erf works when quantile_forest == intermediate_threshold
  expect_equal(
    predict.erf(
      erf_3,
      newdata = X_test_small
    ),
    predict_erf(
      object = erf_3$quantile_forest,
      newdata = X_test_small,
      lambda = erf_3$lambda,
      threshold = erf_3$intermediate_quantile,
      out_of_bag = TRUE
    )$predictions
  )


})

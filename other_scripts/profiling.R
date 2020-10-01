library(profvis)
library(grf)
library(erf)

# params
set.seed(42)
n <- 2e3
p <- 40
distr <- "gaussian"
df <- 2.5
ntest <- 1e4
quantiles_predict <- c(.99, .995, .999, .9995)

# helpers
source("reproduce_paper_results/simulation_functions.R")


# generate training data
dat <- generate_joint_distribution(n = n, p = p, distr = distr, df = df)

# generate test data
x_test <- matrix(0, nrow = ntest, ncol = p)
x_test[, 1] <- seq(-1, 1, length.out = ntest)


# fit quantile regression function w/ grf
fit_grf <- quantile_forest(dat$X, dat$Y)

predictions_erf <- predict_erf(fit_grf, quantiles = quantiles_predict,
                               threshold = .8,
                               newdata = x_test, model_assessment = FALSE,
                               Y.test = NULL,
                               out_of_bag = FALSE)$predictions

predictions_true <- generate_theoretical_quantiles(alpha = quantiles_predict,
                                                   x = x_test[, 1],
                                                   distr = distr, df = df,
                                                   scale = 2)

predictions_grf <- predict(fit_grf, x_test, quantiles = quantiles_predict)


# profile predict_erf
profvis(
  predictions_erf <- predict_erf(fit_grf, quantiles = quantiles_predict,
                                 threshold = .8,
                                 newdata = x_test, model_assessment = FALSE,
                                 Y.test = NULL,
                                 out_of_bag = FALSE)$predictions
)

profvis(
  predictions_grf <- predict(fit_grf, x_test, quantiles = quantiles_predict)

)



# very large test set
load("other_scripts/cpp_test_data2.Rdata")
profvis(
  purrr::map_dfr(1:200, optim_wrap1,init_par, weighted_llh,
                 exc_data, exc_idx, wi_x0)

)

profvis(
  expr = {
  wi_x0_mat <- as.matrix(wi_x0)
  ww <- wi_x0_mat[, exc_idx]
  purrr::map_dfr(1:200, optim_wrap2,init_par, weighted_llh,
                 exc_data, exc_idx, ww)
  }
)

# C++
library(Rcpp)
library(bench)
load("other_scripts/cpp_test_data.Rdata")

sourceCpp("src/weighted_llh.cpp")

mark(
  weighted_llh(data, weights, par),
  weighted_LLH(data, weights, par),
  check = FALSE
)

weighted_llh(data = data, weights = weights, pars = par)
weighted_LLH(data, weights, par)


#
library(randtoolbox)
set.seed(3)
halton(30, 4)

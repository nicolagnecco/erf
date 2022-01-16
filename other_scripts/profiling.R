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

res <- mark(
  weighted_llh(par = par, data = data, weights = weights, lambda = 1,
               xi_prior = -0.3),
  weighted_LLH(data = data, weights = weights, par = par, lambda = 1,
               xi_prior = -0.3)
)

weighted_llh(par = par, data = data, weights = weights, lambda = 1,
             xi_prior = -0.3)
weighted_LLH(data = data, weights = weights, par = par, lambda = 1,
             xi_prior = -0.3)


# Halton's sequence
library(randtoolbox)
set.seed(3)
halton(30, 4)


# Speed
library(tidyverse)

fun1 <- function(i, dat, z) {
  x <- dat[i, ]

  x[[1]] + x[[2]] - z
}

fun2 <- function(x, y, z){
  x + y - z
}

n <- 1e4

tbl <- tibble::tibble(
  x = rnorm(n),
  y = rexp(n)
)

res1 <- function() {
  # rowwise
  # speed: good
  # memory allocation: bad
  tbl %>%
    rowwise() %>%
    mutate(r = fun2(x, y, z)) %>%
    select(r) %>% unlist() %>% unname()
}

res2 <- function() {
  # pmap
  # speed: very good
  # memory allocation: very good
  purrr::pmap_dbl(tbl, fun2, 3)
}

res3 <- function() {
  # map
  # speed: bad
  # memory allocation: very good
  purrr::map_dbl(1:n, function(i)  {
    x <- tbl[i, ]

    x[[1]] + x[[2]] - 3
  })
}


bb <- bench::mark(
  res1(),
  res2(),
  res3(),
  filter_gc = FALSE
)


# Array to lists
W <- grf::get_forest_weights(
  forest = erf_1$quantile_forest,
  newdata = X_test)


bench::mark(
as.matrix(W),
purrr::array_branch(W, 1),
check = FALSE
)

# compare speed of predict_gpd_params vs predict_gpd_params2
predict_gpd_params2 <- function(W, Y, Q, lambda) {
  ## numeric_matrix numeric_vector numeric_vector numeric -> tibble
  ## produce a tibble with MLE GPD scale and shape parameter for each test point
  ## each row corresponds to a test point, each column to a GPD parameter,
  ## namely, `scale` and `shape`

  # compute exceedances
  exc_ind <- which(Y > Q)
  Z <- (Y - Q)[exc_ind]
  W <- W[, exc_ind]
  W <- purrr::array_branch(W, 1)

  # initial guess for GPD parameters
  init_pars <- ismev::gpd.fit(Z, 0, show = FALSE)$mle

  # GPD parameters for each test observation
  purrr::map_dfr(W, optim_wrapper, init_pars, Z, lambda, init_pars[2])
}

W <- as.matrix(grf::get_forest_weights(
  forest = erf_1$quantile_forest,
  newdata = X_test), 1)

Q <- predict_intermediate_quantile(
  erf_1$intermediate_threshold,
  intermediate_quantile = 0.8
)

bench::mark(
predict_gpd_params(W, Y, Q, erf_1$lambda),
predict_gpd_params2(W, Y, Q, erf_1$lambda)
)


# check accuracy of cv with non data-leakage
bench::mark(grf::quantile_forest(X, Y))
bench::mark(grf::quantile_forest(X, Y, num.trees = 50))
plot(X[, 1], predict(intermediate_threshold_1,
                     quantile = .8), ylim = c(0, 3))
plot(X[, 1], predict(intermediate_threshold_1_small,
                     quantile = .8), ylim = c(0, 3))

quantile_forest <- grf::quantile_forest(
  X, Y, min.node.size = 40
)

Q_X <- predict_intermediate_quantile(
  intermediate_threshold_1,
  intermediate_quantile = .8
)
pars1 <- predict_gpd_params(
  quantile_forest = quantile_forest,
  newdata = X_test,
  Q_X = Q_X,
  lambda = 0
)

Q_X_small <- predict_intermediate_quantile(
  intermediate_threshold_1_small,
  intermediate_quantile = .8
)
pars2 <- predict_gpd_params(
  quantile_forest = quantile_forest,
  newdata = X_test,
  Q_X = Q_X_small,
  lambda = 0)


plot(X_test[, 1], pars1$sigma, ylim = c(0.6, 2.2))
plot(X_test[, 1], pars2$sigma, ylim = c(0.6, 2.2))

summary(pars1)
summary(pars2)


# Optimization in C++
library(Rcpp)
library(bench)
load("other_scripts/cpp_test_data.Rdata")

sourceCpp("src/fastoptim.cpp")
res <- bench::mark(
  get_vmin_i(init_pars = par, weights = weights, data = data,
             lambda = 0.1, xi_prior = par[2], intermediate_quantile = 0.8),
  stats::optim(par = par, fn = weighted_llh_test, data = data,
               weights = weights,
               lambda = 0.1, xi_prior = par[2], intermediate_quantile = 0.8),
  check = FALSE
)

View(res)

get_vmin_i(init_pars = par, weights = weights, data = data,
           lambda = 0.1, xi_prior = par[2], intermediate_quantile = 0.8)


# Optimization in C++ loop
library(Rcpp)
library(bench)
load("other_scripts/cpp_test_data2.Rdata")

sourceCpp("src/fastoptim.cpp")

W <- as.matrix(wi_x0)[1:1000, exc_idx, drop = FALSE]


gpd_cpp <- predict_gpd_params_cpp(
  init_pars = init_par, data = exc_data, weights_mat = t(W),
  lambda = 10, xi_prior = .5, intermediate_quantile = 0.8
)

gpd_r <- purrr::map_dfr(seq_len(nrow(W)), function(i) {
  wi_x <- W[i, ]
  optim_wrapper(
    wi_x = wi_x, init_pars = init_par, Z = exc_data,
    lambda = 10, xi_prior = init_par[2], intermediate_quantile = 0.8)
})


plot((gpd_cpp[, 1] - gpd_r$sigma)/ gpd_r$sigma)
plot((gpd_cpp[, 2] - gpd_r$xi) / abs(gpd_r$xi))
plot(gpd_cpp[, 2])
plot(gpd_r$xi)


res <- bench::mark(
  predict_gpd_params_cpp(
    init_pars = init_par, data = exc_data, weights_mat = t(W),
    lambda = 0.1, xi_prior = init_par[2], intermediate_quantile = 0.8
  ),
  purrr::map(seq_len(nrow(W)), function(i) {
    wi_x <- W[i, ]
    optim_wrapper(
      wi_x = wi_x, init_pars = init_par, Z = exc_data,
      lambda = 0.1, xi_prior = init_par[2], intermediate_quantile = 0.8)
  }),
  check = FALSE
)

# Test function
predict_gpd_params_cpp(
  init_pars = init_par, data = exc_data, weights_mat = t(W),
  lambda = 0, xi_prior = init_par[2], intermediate_quantile = 0.8
) %>%
  as_tibble(.name_repair = ~c("sigma", "xi"))

tibble::tibble(
  "sigma" = numeric(0),
  "xi" = numeric(0)
) %>%
  dplyr::bind_rows(
    purrr::map_dfr(seq_len(nrow(W)), function(i) {
      wi_x <- W[i, ]
      optim_wrapper(wi_x, init_par, exc_data, .1, init_par[2],
                    .8)
    })
  )

library(profvis)
library(grf)
library(erf)

# params
n <- 2e3
p <- 40
model <- "gaussian"
df <- 2.5
ntest <- 1e3
quantiles_predict <- c(.9, .99, .999, .9995)

# helpers
generate_joint_distribution <- function(n, p,
                                        model = c("gaussian", "student_t"),
                                        scale = 2, df){
  ## integer integer character numeric integer -> list
  ## generate n iid observations of (X, Y), where X is p-dimensional predictor
  ## and Y is the response. Y has conditional distribution equal to
  ## model = "gaussian" or "student_t" with a certain scale and
  ## with df degree of freedom.
  ## Returns a list with:
  ## - X, nxp matrix, p-dimensional predictor
  ## - Y, vector with n elements, response variable

  model <- match.arg(model)
  X <- matrix(runif(n * p, min = -1, max = 1), n, p)

  switch(model,
         "gaussian" = {
           Y = ifelse(X[,1] < 0, rnorm(n, 0, 1), rnorm(n, 0, scale))
         },
         "student_t" = {
           Y = ifelse(X[,1] < 0, rt(n, df = df),  scale * rt(n, df = df))
         })

  return(list(X = X, Y = Y))

}


generate_theoretical_quantiles <- function(alpha, x, model, df, scale){
  ## numeric_vector numeric_vector character integer numeric -> numeric_matrix
  ## produce conditional alpha-quantiles at the x values for model = "gaussian"
  ## or "student_t" with df degree of freedom and relative scale

  ntest <- length(x)
  nalphas <- length(alpha)
  ntest_negative <- length(which(x < 0))
  ntest_nonnegative <- ntest - ntest_negative

  if(model == "gaussian"){
    q1function <-  function(p) qnorm(p, mean=0, sd=1)
    q2function <-  function(p) qnorm(p, mean=0, sd=scale)
  }
  if(model == "student_t"){
    q1function <- function(p) qt(p, df = df)
    q2function <- function(p) scale * qt(p, df = df)
  }


  q_true <- matrix(NA, nrow = ntest, ncol = nalphas)
  q_true[which(x < 0), ] <- matrix(q1function(alpha), nrow = ntest_negative,
                                   ncol = nalphas, byrow = TRUE)
  q_true[which(x >= 0), ] <- matrix(q2function(alpha), nrow = ntest_nonnegative,
                                    ncol = nalphas, byrow = TRUE)

  return(q_true)
}


# generate training data
dat <- generate_joint_distribution(n = n, p = p, model = model, df = df)

# generate test data
x_test <- matrix(0, nrow = ntest, ncol = p)
x_test[, 1] <- seq(-1, 1, length.out = ntest)


# fit quantile regression function w/ grf
fit_grf <- quantile_forest(dat$X, dat$Y)

profvis(
  predictions_erf <- predict_erf(fit_grf, quantiles = quantiles_predict,
                                 threshold = .8,
                                 newdata = x_test, model_assessment = FALSE,
                                 Y.test = NULL,
                                 out_of_bag = FALSE)$predictions
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
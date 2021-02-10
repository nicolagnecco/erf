## Constants:

n <- 1e3
p <- 10
n_test <- 1e3
min.node.size <- 40
lambda <- 0.001
df <- 4



## Data definitions:

## X is numeric_matrix with n rows and p columns
## interp. matrix with n observations with p predictors
X <- matrix(runif(n * p, min = -1, max = 1), nrow = n, ncol = p)
X_test <- matrix(runif(n_test * p, min = -1, max = 1), nrow = n_test, ncol = p)


## Y is numeric_vector with n elements
## interp. vector of responses
Y <-  (1 + 1 * (X[, 1] > 0)) * rt(n, df)
Y_test <- (1 + 1 * (X_test[, 1] > 0)) * rt(n_test, df)


## quantile_forest is an object from grf::quantile
## interp. a quantile forest
quantile_forest_1 <- grf::quantile_forest(X, Y, min.node.size = min.node.size)


## intermediate_estimator is object of any class such that
## - it has a predict method
## - the second argument of such predict is the numeric_matrix of test predictors
## interp. a fitted object to estimate intermediate quantile
intermediate_estimator_1 <- grf::quantile_forest(X, Y)
intermediate_estimator_2 <- structure(list(), class = "foo")
intermediate_estimator_3 <- structure(list(), class = "lm")
intermediate_estimator_4 <- lm(Y ~ X)


structure(list(), class = "extreme_forest")
## extreme_forest is a named list made of
## - quantile_forest is quantile_forest
## - intermediate_estimator is intermediate_estimator
## - min.node.size is numeric
## - lambda is numeric
## interp. an extreme forest
extreme_forest_1 <- structure(list(
  "quantile_forest" = quantile_forest_1,
  "intermediate_estimator" = intermediate_estimator_1,
  min.node.size = min.node.size,
  lambda = lambda),
  class = "extreme_forest")



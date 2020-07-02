library(grf)
library(tidyverse)

# Train a quantile forest.
n <- 50
p <- 3
X <- matrix(rnorm(n * p), n, p)
Y <- X[, 1] * rnorm(n)
q.forest <- quantile_forest(X, Y, quantiles = c(0.1, 0.5, 0.9), num.trees = 1)
unclass(q.forest)
q.forest$`_drawn_samples`[[1]]
unlist(q.forest$`_leaf_samples`[[1]])
unlist(q.forest$`_leaf_samples`[[1]]) %in% q.forest$`_drawn_samples`[[1]]
x_0 <- matrix(rnorm(1 * p), 1, p)
W2 <- get_sample_weights(q.forest, newdata = x_0) %>% as.matrix()

median(Y[which(W2 != 0)])
predict(q.forest, newdata = x_0, quantile = 0.5)

g_hat <- predict(q.forest, quantile = 0.5)
sort(which(is.nan(g_hat)))
sort(q.forest$`_drawn_samples`[[1]] + 1)
unique(g_hat)

g_hat_2 <- predict(q.forest, newdata = X, quantile = 0.5)
g_hat
g_hat_2

which(W2 != 0) %in% (unlist(q.forest$`_leaf_samples`[[1]]) + 1)


sum(W2)
q.forest$`_drawn_samples`
sort(unlist(q.forest$`_leaf_samples`))
sort(unique(unlist(apply(W2, 1, function(x){which(x !=0) - 1}))))




W <- get_sample_weights(q.forest) %>% as.matrix()
all.equal(W, W2)
W - W2
W
W2[, 35]
W[, 35]

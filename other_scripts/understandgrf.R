library(grf)
library(tidyverse)

# Understand how grf::quantile_forest works
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
W2 <- get_forest_weights(q.forest, newdata = x_0) %>% as.matrix()

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




W <- get_forest_weights(q.forest) %>% as.matrix()
all.equal(W, W2)
W - W2
W
W2[, 35]
W[, 35]


# Test
n <- 500
p <- 10
X <- matrix(rnorm(n * p), n, p)
Y <- X[, 1] * rnorm(n)
object <- quantile_forest(X, Y, quantiles = c(0.1, 0.5, 0.9))
object2 <- object; class(object2) <- "ciaooo"
newdata <-  matrix(rnorm(n * p), n, p)
# newdata <-  matrix(rnorm(n * (p - 1)), n, p - 1)
# newdata <- NULL
quantiles <- c(.99, .999)
threshold <- 0.5
model_assessment <- TRUE
Y.test <- newdata[, 1] * rnorm(n)
out_of_bag <- FALSE
# Y.test <- NULL
# Y.test <- newdata[-1, 1] * rnorm(n - 1)
res <- predict_erf(object, quantiles = quantiles, threshold = threshold,
                   newdata = newdata, model_assessment = TRUE,
                   Y.test = Y.test, out_of_bag = FALSE)

res$predictions
res$plot

# compare with original code
X.train <- X
X.test <- newdata
alpha <- threshold
alpha.new <- quantiles
min.node.size <- 5
fit.grf <- object


weighted.LLH = function(data, weights, par) {
  sig = par[1] # sigma
  xi = par[2] # xi
  y = 1 + (xi/sig) * data
  if (min(sig) <= 0)
    nl = 10^6
  else {
    if (min(y) <= 0)
      nl = 10^6
    else {
      nl = sum(weights*(log(sig) + (1 + 1/xi)*log(y)))
    }
  }
  return(nl)
}

q.GPD = function(q, alpha, u, sigma, xi){
  (((1-q)/(1-alpha))^{-xi} - 1)*sigma/xi + u
}

extreme_forest <- function(X.train, Y, X.test, alpha, alpha.new, Y.test=NULL, grf=FALSE){

  ntest <- nrow(X.test)
  results <- array(NA, dim = c(ntest, length(alpha.new)))
  EVT.par <- array(NA, dim = c(ntest, 2))

  fit.grf = quantile_forest(X.train, Y, quantiles = c(0.1,0.5,0.9), min.node.size = min.node.size) #c(0.7, 0.8, alpha, 0.95)
  q.hat.train = predict(fit.grf, X.train, quantiles = alpha)
  exc.idx = which(Y - q.hat.train > 0)
  exc.data = (Y - q.hat.train)[exc.idx]


  # Estimate GRF and EVT quantiles
  if(grf) results.grf <- predict(fit.grf, X.test, quantiles = alpha.new)



  # Obtaining weights (dimension is ntest x n)
  qrf.weights = get_forest_weights(fit.grf, newdata = X.test, num.threads = NULL)


  #plot(X.train[,1],qrf.weights[24,])

  in.par <- ismev::gpd.fit(exc.data, 0, show=FALSE)$mle
  for(i in 1:ntest){
    EVT.par[i,] = optim(par=in.par, fn=weighted.LLH, data=exc.data, weights=qrf.weights[i,exc.idx])$par
  }
  #plot(X.test[,1],EVT.par[,1,r])

  # Estimate GRF and EVT quantiles
  q.alpha.test = predict(fit.grf, X.test, quantiles = alpha)
  for(j in 1:length(alpha.new)){
    results[,j] <- q.GPD(q = alpha.new[j], alpha=alpha,u  = q.alpha.test, sigma = EVT.par[,1], xi = EVT.par[,2])
  }

  if(!is.null(Y.test)){
    par(mfrow=c(1,3))

    ## plot qq plot for unconditional model, ignoring covariates
    q.hat.uncon <- quantile(Y, probs = alpha)
    exc.uncon.idx = which(Y - q.hat.uncon > 0)
    exc.uncon = (Y - q.hat.uncon)[exc.uncon.idx]
    uncon.par <- gpd.fit(exc.uncon, 0, show=FALSE)$mle

    exc.test.uncon.idx = which(Y.test -  q.hat.uncon > 0)
    exc.test.uncon = (Y.test - q.hat.uncon)[exc.test.uncon.idx]
    n.exc.test.uncon = length(exc.test.uncon)
    exc.test.uncon.std <- 1/uncon.par[2] * log(1+uncon.par[2] * exc.test.uncon/uncon.par[1])
    plot(qexp((1:n.exc.test.uncon)/ (n.exc.test.uncon+1)), sort(exc.test.uncon.std))
    abline(0,1)

    ## plot qq plot for model that uses covariates only for the alpha-threshold
    exc.test.idx = which(Y.test -  q.alpha.test > 0)
    exc.test = (Y.test - q.alpha.test)[exc.test.idx]
    n.exc.test = length(exc.test)

    exc.test.std2 <- 1/in.par[2] * log(1+in.par[2] * exc.test/in.par[1])
    plot(qexp((1:n.exc.test)/ (n.exc.test+1)), sort(exc.test.std2))
    abline(0,1)


    ## plot qq plot for full covariate model for alpha-threshold and GPD parameters
    exc.test.std <- 1/EVT.par[exc.test.idx,2] * log(1+EVT.par[exc.test.idx,2] * exc.test/EVT.par[exc.test.idx,1])
    plot(qexp((1:(n.exc.test-1))/ (n.exc.test)), sort(exc.test.std))
    abline(0,1)



  }

  if(!grf)
    return(list(results=results, EVT.par=EVT.par, X.train=X.train, Y=Y, X.test=X.test))
  else
    return(list(results=results, results.grf=results.grf, EVT.par=EVT.par, X.train=X.train, Y=Y, X.test=X.test))
}


library(qqplotr)
library(ggplot2)
dat <- data.frame(y = rexp(50))

di <- "exp" # exponential distribution
dp <- list(rate = 1) # exponential rate parameter

gg <- ggplot(data = dat, mapping = aes(sample = y)) +
  stat_qq_band(distribution = di, dparams = dp) +
  stat_qq_line(distribution = di, dparams = dp) +
  stat_qq_point(distribution = di, dparams = dp) +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

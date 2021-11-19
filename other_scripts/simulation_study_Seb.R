library(grf)
library(ggplot2)
library(ismev)



## nsim: number of simulations
## n: number of samples in each simulations
## p: ....

sim.RF.pred <- function(nsim, n, p, alpha, alpha.new, model, methods, min.node.size, dof.t){
  ntest = 1000
  X.test = matrix(0, ntest, p) # matrix(runif(ntest*p, min = -1, max = 1), ntest, p) #

  X.test[,1] = seq(-.2, 1, length.out = ntest)
  results <- array(NA, dim = c(ntest, length(alpha.new), length(methods), nsim))
  EVT.par <- array(NA, dim = c(ntest, 2, nsim))

  Y.test =  ifelse(X.test[,1] < 0, rnorm(n, 0, 1), rnorm(n, 0, 2)) #ifelse(X.test[,1] < 0, rt(n, df = df), m* rt(n, df = df))



  if(model == "gaussian"){
    q1function = function(p) qnorm(p, mean=0, sd=1)
    q2function = function(p) qnorm(p, mean=0, sd=2)
  }
  if(model == "t"){
    df = dof.t
    m = 2
    q1function = function(p) qt(p, df=df)
    q2function = function(p) m*qt(p, df=df)
  }


  q.true = array(NA, dim = c(ntest, length(alpha.new)))
  q.true[which(X.test[,1]<0),] = matrix(sapply(alpha.new, q1function), nrow=length(which(X.test[,1]<0)), ncol=3, byrow = TRUE)
  q.true[which(X.test[,1]>0),] = matrix(sapply(alpha.new, q2function), nrow=length(which(X.test[,1]>0)), ncol=3, byrow = TRUE)


  pb = txtProgressBar(min = 0, max = nsim, initial = 0, style = 3)
  for(r in 1:nsim){
    setTxtProgressBar(pb,r)

    X.train = matrix(runif(n*p, min = -1, max = 1), n, p)
    if(model == "gaussian"){
      Y = ifelse(X.train[,1] < 0, rnorm(n, 0, 1), rnorm(n, 0, 2))
    }
    if(model == "t"){
      Y = ifelse(X.train[,1] < 0, rt(n, df = df), m* rt(n, df = df))
    }


    # Estimate GRF and EVT quantiles
    erf.fit <- extreme_forest(X.train, Y, X.test, alpha, alpha.new, Y.test = NULL, grf=TRUE)
    results[,,1,r] <- erf.fit$results.grf
    results[,,2,r] <- erf.fit$results
    EVT.par[,,r] <- erf.fit$EVT.par

  }
  return(list(results=results, q.true=q.true, EVT.par=EVT.par, X.test=X.test))
}



set.seed(1234)
undebug(sim.RF.pred)

nsim <- 10
n = 2000
p = 40
alpha = 0.8
model = "gaussian"
dof.t = 3 #this is the tail index of the data
min.node.size <- 5
alpha.new = c(.99, .999, .9995)
methods <- c("GRF", "RF_EVT")

res <- sim.RF.pred(nsim, n, p, alpha, alpha.new, model,
                   methods, min.node.size, dof.t)
#save(res, file = "sim_results/res_gauss_n2000_p40_alpha08_nsim50_node5.Rdata")
load("sim_results/res_t_n2000_p40_alpha08_nsim100_node50.Rdata")


ntest <- dim(res$results)[1]
nsim <- dim(res$results)[4]

a <- 0.1
# take mean quantiles across simulations for each test observation, quantile, and method
res.mean <- apply(res$results, FUN = mean, MARGIN = c(1,2,3))
# take upper empirical
res.CI.upper <- apply(res$results, FUN = quantile, MARGIN = c(1,2,3), probs=1-a)
res.CI.lower <- apply(res$results, FUN = quantile, MARGIN = c(1,2,3), probs=a)

res.diff2 <- (res$results - array(res$q.true, dim = c(ntest, length(alpha.new), length(methods), nsim)))^2
res.rmse <- apply(res.diff2, FUN = function(x) sqrt(mean(x)), MARGIN = c(1,2,3))

EVT.par.mean <- apply(res$EVT.par, FUN = mean, MARGIN = c(1,2))
EVT.par.CI.upper <- apply(res$EVT.par, FUN = quantile, MARGIN = c(1,2), probs=1-a)
EVT.par.CI.lower <- apply(res$EVT.par, FUN = quantile, MARGIN = c(1,2), probs=a)



j=3
ggplot() + aes(x = res$X.test[,1]) + labs(x = "X", y = paste(alpha.new[j]*100, "%-Quantile", sep="")) +
  geom_ribbon(aes(ymin = res.CI.lower[,j,1], ymax = res.CI.upper[,j,1], fill = "GRF"), alpha=.5) +
  geom_ribbon(aes(ymin = res.CI.lower[,j,2], ymax = res.CI.upper[,j,2], fill = "EVT"), alpha=.5) +
  geom_line(aes(x = res$X.test[,1], y = res$q.true[,j]), col = "black", lwd = 0.5) +
  geom_line(aes(x = res$X.test[,1], y = res.mean[,j,1]), col = "blue", lwd = 0.5) +
  geom_line(aes(x = res$X.test[,1], y = res.mean[,j,2]), col = "red", lwd = 0.5)


ggplot() + aes(x = res$X.test[,1]) + labs(x = "X", y = paste("RMSE", sep="")) +
  geom_line(aes(x = res$X.test[,1], y = res.rmse[,j,1]), col = "blue", lwd = 0.5) +
  geom_line(aes(x = res$X.test[,1], y = res.rmse[,j,2]), col = "red", lwd = 0.5)



ggplot() + aes(x = res$X.test[,1]) + labs(x = "X", y = paste("scale parameter")) +
  geom_ribbon(aes(ymin = EVT.par.CI.lower[,1], ymax = EVT.par.CI.upper[,1], fill = ""), alpha=.5) +
  geom_line(aes(x = res$X.test[,1], y = EVT.par.mean[,1]), col = "black", lwd = 0.5)


ggplot() + aes(x = res$X.test[,1]) + labs(x = "X", y = paste("shape parameter")) +
  geom_ribbon(aes(ymin = EVT.par.CI.lower[,2], ymax = EVT.par.CI.upper[,2], fill = ""), alpha=.5) +
  geom_line(aes(x = res$X.test[,1], y = EVT.par.mean[,2]), col = "black", lwd = 0.5)



### Compare regression fit with unconditional fit

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

  in.par <- gpd.fit(exc.data, 0, show=FALSE)$mle
  for(i in 1:ntest){
    EVT.par[i,] = optim(par=in.par, fn=weighted.LLH, data=exc.data, weights=qrf.weights[i,exc.idx])$par
  }
  #plot(X.test[,1],EVT.par[,1,r])

  # Estimate GRF and EVT quantiles
  q.alpha.test = predict(fit.grf, X.test, quantiles = alpha)
  for(j in 1:length(alpha.new)){
    results[,j] <- q.GPD(q = alpha.new[j], alpha=alpha, u = q.alpha.test,
                         sigma = EVT.par[,1], xi = EVT.par[,2])
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
    plot(qexp((1:n.exc.test)/ (n.exc.test+1)), sort(exc.test.std))
    abline(0,1)



  }

  if(!grf)
    return(list(results=results, EVT.par=EVT.par, X.train=X.train, Y=Y, X.test=X.test))
  else
    return(list(results=results, results.grf=results.grf, EVT.par=EVT.par, X.train=X.train, Y=Y, X.test=X.test))
}

extreme_forest(X.train, Y, X.test=X.train, Y.test = Y.test, alpha, alpha.new)


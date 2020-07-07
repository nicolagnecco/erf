#' Extreme predictions with a quantile forest
#'
#' Gets estimates of the extrapolated conditional quantiles of Y given X
#' using a trained forest.
#'
#' @param object Quantile forest object. The trained forest.
#' @param newdata Numeric matrix.
#'                Rows contain observation at which predictions should be made.
#'                If NULL, makes out-of-bag predictions on the training set
#'                instead (i.e., provides predictions at Xi using only trees
#'                that did not use the i-th training example). Note
#'                that this matrix should have the number of columns as the
#'                training matrix, and that the columns must appear in the same
#'                order. This argument is needed if
#'                \code{model_assessment = TRUE}.
#'                Default is \code{NULL}.
#' @param quantiles Numeric vector (0, 1).
#'                  Extreme quantiles at which estimates are required.
#'                  If NULL, the quantiles used to train the forest is used.
#'                  Default is \code{NULL}.
#' @param threshold Numeric vector (0, 1). Intermediate quantile used to compute
#'                  thresholds \eqn{t(x_i)} for the GPD.
#'                  Default is 0.8.
#' @param model_assessment Boolean. Assess GPD fit with QQ-plot against
#'                         exponential distribution?
#'                         Default is \code{FALSE}.
#' @param Y.test Numeric vector. Responses for the test set, needed
#'               if \code{model_assessment = TRUE}.
#'               Default is \code{NULL}.
#' @param out_of_bag Boolean. Use out-of-bag observations to compute thresholds
#'                   \eqn{t(x_i)} for the GPD?
#'                   Default is \code{FALSE}.
#'
#' @return Predictions at each test point for each desired quantile.
#'
#' @export
predict_erf <- function(object, newdata = NULL, quantiles = NULL,
                        threshold = 0.8, model_assessment = FALSE,
                        Y.test = NULL, out_of_bag = FALSE) {

  validate_inputs(...)

  X0 <- set_test_observations(...)

  wi_x0 <-  grf::get_sample_weights(object, newdata = X0, num.threads = NULL)

  t_xi <- compute_thresholds(object, out_of_bag)

  gpd_pars <- fit_conditional_gpd(object, wi_x0, t_xi)

  q_hat <- compute_extreme_quantiles(gpd_pars, X0, quantiles, threshold)

  if (model_assessment){
    compute_model_assessment(...)
  }
}

wishlist <- function(){
  if (model_assessment){
    if(is.null(newdata) || is.null(Y.test)){
      stop("When model_assessment = TRUE, newdata and Y.test must be supplied.")
    }
  }

  if (is.null(quantiles)) {
    quantiles <- object[["quantiles.orig"]]
  } else {
    if (!is.numeric(quantiles) || length(quantiles) < 1) {
      stop("Error: Must provide numeric quantiles")
    } else if (min(quantiles) <= 0 || max(quantiles) >= 1) {
      stop("Error: Quantiles must be in (0, 1)")
    }
  }

  if (!is.null(newdata)) {
    validate_newdata(newdata, object$X.orig, allow.na = TRUE)
    test.data <- create_test_matrices(newdata)
    do.call.rcpp(quantile_predict, c(train.data, test.data, args))
  } else {
    do.call.rcpp(quantile_predict_oob, c(train.data, args))
  }
}


weighted.LLH <- function(data, weights, par) {
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

q.GPD <- function(q, alpha, u, sigma, xi){
  (((1-q)/(1-alpha))^{-xi} - 1)*sigma/xi + u
}


draft <- function(){






  ntest <- nrow(X.test)
  results <- array(NA, dim = c(ntest, length(alpha.new)))
  EVT.par <- array(NA, dim = c(ntest, 2))

  fit.grf = grf::quantile_forest(X.train, Y, quantiles = c(0.1,0.5,0.9),
                                 min.node.size = min.node.size, num.trees = 2, honesty = FALSE) #c(0.7, 0.8, alpha, 0.95)
  q.hat.train2 = stats::predict(fit.grf, X.train, quantiles = alpha)
  q.hat.train2 = stats::predict(fit.grf, quantiles = alpha)
  exc.idx = which(Y - q.hat.train > 0)
  exc.data = (Y - q.hat.train)[exc.idx]


  # Estimate GRF and EVT quantiles
  if (grf){
    results.grf <- stats::predict(fit.grf, X.test, quantiles = alpha.new)
  }



  # Obtaining weights (dimension is ntest x n)
  qrf.weights = grf::get_sample_weights(fit.grf, newdata = X.test,
                                        num.threads = NULL)


  #plot(X.train[,1],qrf.weights[24,])

  in.par <- ismev::gpd.fit(exc.data, 0, show=FALSE)$mle
  for(i in 1:ntest){
    EVT.par[i,] = stats::optim(par=in.par, fn=weighted.LLH, data=exc.data,
                               weights=qrf.weights[i,exc.idx])$par
  }
  #plot(X.test[,1],EVT.par[,1,r])

  # Estimate GRF and EVT quantiles
  q.alpha.test = stats::predict(fit.grf, X.test, quantiles = alpha)
  for(j in 1:length(alpha.new)){
    results[,j] <- q.GPD(q = alpha.new[j], alpha=alpha,
                         u = q.alpha.test, sigma = EVT.par[,1], xi = EVT.par[,2])
  }

  # if(!is.null(Y.test)){
  #   par(mfrow=c(1,3))
  #
  #   ## plot qq plot for unconditional model, ignoring covariates
  #   q.hat.uncon <- quantile(Y, probs = alpha)
  #   exc.uncon.idx = which(Y - q.hat.uncon > 0)
  #   exc.uncon = (Y - q.hat.uncon)[exc.uncon.idx]
  #   uncon.par <- gpd.fit(exc.uncon, 0, show=FALSE)$mle
  #
  #   exc.test.uncon.idx = which(Y.test -  q.hat.uncon > 0)
  #   exc.test.uncon = (Y.test - q.hat.uncon)[exc.test.uncon.idx]
  #   n.exc.test.uncon = length(exc.test.uncon)
  #   exc.test.uncon.std <- 1/uncon.par[2] * log(1+uncon.par[2] * exc.test.uncon/uncon.par[1])
  #   plot(qexp((1:n.exc.test.uncon)/ (n.exc.test.uncon+1)), sort(exc.test.uncon.std))
  #   abline(0,1)
  #
  #   ## plot qq plot for model that uses covariates only for the alpha-threshold
  #   exc.test.idx = which(Y.test -  q.alpha.test > 0)
  #   exc.test = (Y.test - q.alpha.test)[exc.test.idx]
  #   n.exc.test = length(exc.test)
  #
  #   exc.test.std2 <- 1/in.par[2] * log(1+in.par[2] * exc.test/in.par[1])
  #   plot(qexp((1:n.exc.test)/ (n.exc.test+1)), sort(exc.test.std2))
  #   abline(0,1)
  #
  #
  #   ## plot qq plot for full covariate model for alpha-threshold and GPD parameters
  #   exc.test.std <- 1/EVT.par[exc.test.idx,2] * log(1+EVT.par[exc.test.idx,2] * exc.test/EVT.par[exc.test.idx,1])
  #   plot(qexp((1:n.exc.test)/ (n.exc.test+1)), sort(exc.test.std))
  # abline(0,1)
  #
  #
  #
  # }

  if (!grf){
    return(list(results=results, EVT.par=EVT.par, X.train=X.train,
                Y=Y, X.test=X.test))
  } else {
    return(list(results=results, results.grf=results.grf,
                EVT.par=EVT.par, X.train=X.train, Y=Y, X.test=X.test))
  }
}

#' Predict with a quantile forest
#'
#' Gets estimates of the conditional quantiles of Y given X using a trained forest.
#'
#' @param object The trained forest.
#' @param newdata Points at which predictions should be made. If NULL, makes out-of-bag
#'                predictions on the training set instead (i.e., provides predictions at
#'                Xi using only trees that did not use the i-th training example). Note
#'                that this matrix should have the number of columns as the training
#'                matrix, and that the columns must appear in the same order.
#' @param quantiles Vector of quantiles at which estimates are required. If NULL, the quantiles
#'  used to train the forest is used. Default is NULL.
#' @param num.threads Number of threads used in training. If set to NULL, the software
#'                    automatically selects an appropriate amount.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Predictions at each test point for each desired quantile.
#'
#' @examples
#' \donttest{
#' # Train a quantile forest.
#' n <- 50
#' p <- 10
#' X <- matrix(rnorm(n * p), n, p)
#' Y <- X[, 1] * rnorm(n)
#' q.forest <- quantile_forest(X, Y, quantiles = c(0.1, 0.5, 0.9))
#'
#' # Predict on out-of-bag training samples.
#' q.pred <- predict(q.forest)
#'
#' # Predict using the forest.
#' X.test <- matrix(0, 101, p)
#' X.test[, 1] <- seq(-2, 2, length.out = 101)
#' q.pred <- predict(q.forest, X.test)
#' }
#'
#' @method predict quantile_forest
#' @export
predict.quantile_forest <- function(object,
                                    newdata = NULL,
                                    quantiles = NULL,
                                    num.threads = NULL, ...) {
  if (is.null(quantiles)) {
    quantiles <- object[["quantiles.orig"]]
  } else {
    if (!is.numeric(quantiles) || length(quantiles) < 1) {
      stop("Error: Must provide numeric quantiles")
    } else if (min(quantiles) <= 0 || max(quantiles) >= 1) {
      stop("Error: Quantiles must be in (0, 1)")
    }
  }

  # If possible, use pre-computed predictions.
  quantiles.orig <- object[["quantiles.orig"]]
  if (is.null(newdata) && identical(quantiles, quantiles.orig) && !is.null(object$predictions)) {
    return(object$predictions)
  }

  num.threads <- grf:::validate_num_threads(num.threads)
  forest.short <- object[-which(names(object) == "X.orig")]
  X <- object[["X.orig"]]
  train.data <- grf:::create_train_matrices(X, outcome = object[["Y.orig"]])

  args <- list(forest.object = forest.short,
               quantiles = quantiles,
               num.threads = num.threads)

  if (!is.null(newdata)) {
    validate_newdata(newdata, object$X.orig, allow.na = TRUE)
    test.data <- create_test_matrices(newdata)
    do.call.rcpp(quantile_predict, c(train.data, test.data, args))
  } else {
    do.call.rcpp(quantile_predict_oob, c(train.data, args))
  }
}

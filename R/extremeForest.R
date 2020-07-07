#' Extreme predictions with a quantile forest
#'
#' Gets estimates of the extrapolated conditional quantiles of Y given X
#' using a trained forest.
#'
#' @param object Quantile forest object. The trained forest.
#' @param quantiles Numeric vector (0, 1).
#'                  Extreme quantiles at which estimates are required.
#' @param threshold Numeric (0, 1). Intermediate quantile used to compute
#'                  thresholds \eqn{t(x_i)} for the GPD.
#'                  Default is 0.8.
#'                  Note that \code{threshold} < \code{quantiles}.
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
predict_erf <- function(object, quantiles, threshold = 0.8,
                        newdata = NULL, model_assessment = FALSE,
                        Y.test = NULL, out_of_bag = FALSE) {

  validate_inputs(object, quantiles, threshold, newdata, model_assessment,
                  Y.test, out_of_bag)

  X0 <- set_test_observations(object, newdata)

  wi_x0 <-  grf::get_sample_weights(object, newdata = X0, num.threads = NULL)

  t_xi <- compute_thresholds(object, threshold = threshold,
                             X = object$X.orig, out_of_bag = out_of_bag)

  gpd_pars <- fit_conditional_gpd(object, wi_x0, t_xi)

  q_hat <- compute_extreme_quantiles(gpd_pars, X0, quantiles, threshold)

  if (model_assessment) compute_model_assessment(q_hat, newdata, Y.test)
}

validate_inputs <- function(object,  quantiles, threshold, newdata,
                            model_assessment, Y.test, out_of_bag){
  ## ... -> boolean
  ## check whether inputs are well-formed

  check_object(object)

  check_newdata_object(newdata, object)

  check_quantiles_thres(quantiles, threshold)

  check_model_assessment(model_assessment, newdata, Y.test)

  return(TRUE)
}

check_object <- function(object){
  ## quantile_forest -> boolean
  ## check whether object is of class "quantile_forest"

  if(class(object)[1] != "quantile_forest"){
    stop("object must be of class 'quantile_forest'")
  }

  return(TRUE)
}

check_newdata_object <- function(newdata, object){
  ## numeric_matrix, quantile_forest -> boolean
  ## check whether newdata and object are well-formed

  if (!is.null(newdata)) {
    if(class(newdata) != "matrix"){
      stop("newdata must be of class 'matrix'")
    }

    if (ncol(newdata) != ncol(object$X.orig)) {
      stop("newdata must have the same number of columns as the training matrix")
    }
  }

  return(TRUE)
}

check_quantiles_thres <- function(quantiles, threshold){
  ## numeric_vector_(0, 1), numeric_(0, 1) -> boolean
  ## check whether quantiles and threshold are well-formed

  if (!is.numeric(quantiles) || length(quantiles) < 1) {
    stop("must provide numeric quantiles")
  } else if (min(quantiles) <= 0 || max(quantiles) >= 1) {
    stop("quantiles must be in (0, 1)")
  }

  if (!is.numeric(threshold) || length(threshold) < 1) {
    stop("must provide numeric threshold")
  } else if (min(threshold) <= 0 || max(threshold) >= 1) {
    stop("threshold must be in (0, 1)")
  }

  if (any(quantiles < threshold)){
    stop("all quantiles must be larger than threshold")
  }

  return(TRUE)

}

check_model_assessment <- function(model_assessment, newdata, Y.test){
  ## boolean, numeric_matrix, numeric_vector -> boolean
  ## check whether inputs are well formed

  if (model_assessment){
    if(is.null(newdata) || is.null(Y.test)){
      stop("newdata and Y.test must be supplied when model_assessment = TRUE")
    }
  }

  if (!is.null(newdata) && !is.null(Y.test)){
    valid.classes <- c("matrix", "numeric")

    if(!class(Y.test) %in% valid.classes){
      stop(paste("Y.test must be one of the following classes:",
           paste(valid.classes, collapse = ", "), sep = " "))
    }

    n_ytest <- if(class(Y.test) == "numeric"){
      length(Y.test)
    } else {
      nrow(Y.test)
    }

    if (n_ytest != nrow(newdata)){
      stop("newdata and Y.test must have the same number of observations")
    }
  }

  return(TRUE)

}

set_test_observations <- function(object, newdata){
  ## quantile_forest numeric_matrix -> numeric_matrix
  ## set test observation matrix

  if(is.null(newdata)){
    X0 <- object$X.orig
  } else {
    X0 <- newdata
  }

  return(X0)
}

compute_thresholds <- function(object, threshold, X, out_of_bag = FALSE){
  ## quantile_forest numeric(0, 1) numeric_matrix boolean -> numeric_vector
  ## compute conditional quantile based on quantile_forest object

  if (out_of_bag){
    q_hat <- stats::predict(object, quantiles = threshold)
  } else {
    q_hat <- stats::predict(object, newdata = X, quantiles = threshold)
  }

  return(q_hat)
}

fit_conditional_gpd <- function(object, wi_x0, t_xi){
  ## quantile_forest numeric_matrix numeric_vector -> matrix
  ## produce matrix with MLE GPD scale and shape parameter for each test point

  ntest <- nrow(wi_x0)
  Y <- object$Y.orig
  exc_idx = which(Y - t_xi > 0)
  exc_data = (Y - t_xi)[exc_idx]

  init_par <- ismev::gpd.fit(exc_data, 0, show=FALSE)$mle
  EVT_par <- array(NA, dim = c(ntest, 2))

  for(i in seq_len(ntest)){
    EVT_par[i,] = stats::optim(par = init_par, fn = weighted_LLH, data=exc_data,
                               weights=wi_x0[i, exc_idx])$par
  }

  return(EVT_par)
}

compute_extreme_quantiles <- function(...){
  # !!! write signature & purpose
}

compute_model_assessment <- function(...){
  # !!! write signature & purpose
}

weighted_LLH <- function(data, weights, par) {
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

q_GPD <- function(q, alpha, u, sigma, xi){
  (((1-q)/(1-alpha))^{-xi} - 1)*sigma/xi + u
}


# draft ###########

draft <- function(X.train, Y, X.test, alpha, alpha.new, Y.test=NULL, grf=FALSE,
                  min.node.size = 5, q.hat.train = 0){

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



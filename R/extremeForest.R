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
#'                  Note that \code{threshold} < \code{quantiles}.
#'                  Default is 0.8.
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
#' @return List. The list is made of:
#' \enumerate{
#' \item Numeric matrix. Predictions at each test point (on rows) for
#'         each desired quantile (on columns).
#' \item Plot if \code{model_assessment = TRUE}. QQ-plot for model assessment.
#' # !!! return also t(x_0), sigma(x_0), csi(x_0), for all x_0 in test_data
#' }
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
  t_x0 <- compute_thresholds(object, threshold = threshold, X = X0)

  gpd_pars <- fit_conditional_gpd(object, wi_x0, t_xi)

  q_hat <- compute_extreme_quantiles(gpd_pars, t_x0, quantiles, threshold)

  if (model_assessment){
    p <- compute_model_assessment(t_x0, Y.test, gpd_pars)
    return(list(predictions = q_hat, plot = p))
  } else {
    return(list(predictions = q_hat))
  }
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
    if(!("matrix" %in% class(newdata))){
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
  ## quantile_forest numeric(0, 1) numeric_matrix boolean -> numeric_matrix
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
  browser()
  init_par <- ismev::gpd.fit(exc_data, 0, show=FALSE)$mle
  EVT_par <- purrr::map_dfr(1:ntest, optim_wrap1,init_par, weighted_llh,
                            exc_data, exc_idx, wi_x0)

  return(as.matrix(EVT_par))
}

optim_wrap1 <- function(i, init_par, obj_fun, exc_data, exc_idx, wi_x0){
  res <- stats::optim(par = init_par, fn = obj_fun, data=exc_data,
                    weights=wi_x0[i, exc_idx])$par
  names(res) <- c("par1", "par2")
  cat("Simulation", i, "\r")
  return(res)
}

compute_extreme_quantiles <- function(gpd_pars, t_x0, quantiles, threshold){
  ## numeric_matrix numeric_matrix numeric_vector(0, 1)
  ## numeric(0, 1) -> numeric_matrix
  ## produce matrix with estimated extremes quantiles. The value at (i, j) gives
  ## the estimated quantile j for test sample i

  res <- matrix(nrow = nrow(t_x0), ncol = length(quantiles))

  for(j in seq_along(quantiles)){
    res[, j] <- q_GPD(p = quantiles[j], p0 = threshold, t_x0 = t_x0,
                      sigma = gpd_pars[, 1], xi = gpd_pars[, 2])
  }

  colnames(res) <- paste("quantile = ", quantiles)

  return(res)
}

compute_model_assessment <- function(t_x0, Y.test, gpd_pars){
  ## numeric_matrix numeric_vector numeric_matrix -> plot
  ## produce QQ-plot of standardized estimated quantiles vs exponential data

  Y <- Y.test
  exc_idx = which(Y - t_x0 > 0)
  exc_data = (Y - t_x0)[exc_idx]
  xi <- gpd_pars[exc_idx, 2]
  sigma <- gpd_pars[exc_idx, 1]

  pseudo_obs <- compute_pseudo_observations(exc_data, sigma, xi)
  n <- length(pseudo_obs)

  observed_quantiles <- sort(pseudo_obs)
  theoretical_quantiles <- stats::qexp((1:n)/ (n + 1))

  plot_model_assessment(theoretical_quantiles, observed_quantiles)

}

compute_pseudo_observations <- function(exc_data, sigma, xi){
  ## numeric_vector numeric numeric -> numeric_vector
  ## compute pseudo observation on exponential margin

  tmp_res <- 1 + xi * exc_data / sigma
  idx <- which(tmp_res <= 0)

  if (length(idx) == 1){
    msg <- paste("Observation", idx, "is not plotted because",
                 "it exceeds its upper end point")
    warning(msg)
  } else if (length(idx) > 1) {
    msg <- paste("Observations", paste(idx, collapse = ", "),
                 "are not plotted because",
                 "they exceed their upper end points")
    warning(msg)
  }

  pseudo_obs <- 1 / xi[tmp_res > 0] * log(tmp_res[tmp_res > 0])
  return(pseudo_obs)


}

plot_model_assessment <- function(x, y){
  ## numeric_vector numeric_vector -> plot
  ## produce QQ-plot

  my_colors <- c("#0072B2", "#D55E00", "#CC79A7")

  dat_plot <- data.frame(theoretical_quantiles = x,
                         observed_quantiles = y)

  dat_random <- sample_exponentials(dat_plot)

  ggplot2::ggplot() +
    ggplot2::geom_line(data = dat_random,
                       ggplot2::aes_string(x = "theoretical_quantiles",
                                           y = "random_exp",
                                           group = "rep"),
                       alpha = 0.2, col = "#D55E00") +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         linetype = "dashed", size = 1, col = "#0072B2") +
    ggplot2::geom_point(data = dat_plot,
                        ggplot2::aes_string(x = "theoretical_quantiles",
                                            y = "observed_quantiles"),
                        size = 1) +

    ggplot2::ggtitle("QQ-plot") +
    ggplot2::xlab("theoretical quantiles") +
    ggplot2::ylab("observed quantiles") +
    ggplot2::theme_bw()

}

sample_exponentials <- function(dat_plot){
  ## data_frame -> data_frame
  ## produce data_frame with 100 randomly sampled exponential observations

  n <- nrow(dat_plot)


  dat <- data.frame(random_exp = double(), theoretical_quantiles = double(),
                    rep = double())
  for (i in 1:100) {
    random_exp <- sort(stats::rexp(n = n))
    theoretical_quantiles <- sort(dat_plot$theoretical_quantiles)

    dat <- rbind(dat,
                 data.frame(random_exp, theoretical_quantiles,
                            rep = rep(i, n)))
  }

  return(dat)
}

weighted_LLH <- function(par, data, weights) {
  ## numeric_vector numeric_matrix numeric_vector -> numeric
  ## returns the weighted GPD log-likelihood

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

q_GPD <- function(p, p0, t_x0, sigma, xi){
  ## numeric(0, 1) numeric(0, 1) numeric_matrix numeric_vector
  ## numeric_vector -> numeric_vector
  ## produce the estimated extreme quantiles for the test samples

  (((1-p)/(1-p0))^{-xi} - 1) * (sigma / xi) + t_x0
}

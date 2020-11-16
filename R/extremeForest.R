#' Extreme predictions with a quantile forest
#'
#' Predicts high conditional quantiles of Y given X using a quantile forest
#' fitted using \code{\link[grf]{quantile_forest}}.
#'
#' @param object Quantile forest object. The trained forest obtained with
#'               \code{\link[grf]{quantile_forest}}.
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
#' @param param_est Character. One of "ML" and "Hill". Specifies the method to
#'                  fit the shape and scale parameter of the GDP distribution.
#'                  "ML" is the weighted maximum likelihood approach proposed by
#'
#'                  "Hill" is the weighted Hill's estimator approach proposed
#'                  by
#' @param lambda Numeric (>= 0). Penalty coefficient for the \eqn{\xi}{csi} parameter
#'               in the weighted log-likelihood.
#'               Default is \code{0}.
#'
#' @return Named list. The list is made of:
#' \itemize{
#' \item \code{predictions} --- Numeric matrix. Predictions at each test point (on rows) for
#'         each desired quantile (on columns).
#' \item \code{pars} --- Numeric matrix. Estimated parameters at each test point (on rows).
#'         The columns contain the estimates for the \eqn{\sigma}{sigma} and
#'         \eqn{\xi}{csi} parameter, respectively.
#' \item \code{threshold} --- Numeric matrix. Intermediate thresholds at each test point (on rows),
#'         estimated using \code{\link[grf]{quantile_forest}}.
#' \item \code{lambda} --- Numeric. Penalty coefficient for the \eqn{\xi}{csi} parameter
#'         in the weighted log-likelihood.
#' \item \code{max_weight} --- Numeric. Maximum value for GRF weights used in the
#'       weighted log-likelihood.
#'       !!! remove this option
#' \item \code{plot} (if \code{model_assessment = TRUE}). QQ-plot for model assessment.
#' }
#'
#' @export
predict_erf <- function(object, quantiles, threshold = 0.8,
                        newdata = NULL, model_assessment = FALSE,
                        Y.test = NULL, out_of_bag = FALSE,
                        param_est = c("ML", "Hill"), lambda = 0){

  predict_erf_internal(object, quantiles, threshold,
                       newdata, model_assessment, Y.test, out_of_bag, lambda,
                       max_weight)

}

predict_erf_internal <- function(object, quantiles, threshold = 0.8,
                        newdata = NULL, model_assessment = FALSE,
                        Y.test = NULL, out_of_bag = FALSE,
                        param_est = c("ML", "Hill"),
                        lambda = 0,
                        wi_x0 = NULL,
                        t_xi = NULL, t_x0 = NULL, t_x0_2) {

  ## same inputs as predict_erf + weights, t_xi, t_x0 -> same output as predict_erf
  ## same purpose as predict_erf

  validate_inputs(object, quantiles, threshold, newdata, model_assessment,
                  Y.test, out_of_bag, lambda, max_weight)
  # !!! add here param_est

  X0 <- set_test_observations(object, newdata)

  if (is.null(wi_x0)){
  wi_x0 <-  as.matrix(grf::get_sample_weights(object, newdata = X0,
                                              num.threads = NULL))
  }

  wi_x0[wi_x0 > max_weight] <- max_weight
  # !!! remove this

  if (is.null(t_xi)){
  t_xi <- compute_thresholds(object, threshold = threshold,
                             X = object$X.orig, out_of_bag = out_of_bag)
  }

  if (is.null(t_x0)){
  t_x0 <- compute_thresholds(object, threshold = threshold, X = X0)
  }

  if (is.null(t_x0_2) & param_est == "Hill"){
    threshold2 <- 1 - 2 * (1 - threshold)
    t_x0_2 <- compute_thresholds(object, threshold = threshold2, X = X0)
  }

  if (param_est == "ML"){
    gpd_pars <- fit_conditional_gpd(object, wi_x0, t_xi, lambda)

  } else if (param_est == "Hill"){
    gpd_pars <- fit_param_hill(object, wi_x0, t_xi, t_x0, t_x0_2, threshold)
  }

  q_hat <- compute_extreme_quantiles(gpd_pars, t_x0, quantiles, threshold)

  if (model_assessment){
    p <- compute_model_assessment(t_x0, Y.test, gpd_pars)
    return(list(predictions = q_hat, pars = gpd_pars, threshold = t_x0,
                lambda = lambda, max_weight = max_weight,
                plot = p)) # !!! do not return lambda when param_est == "Hill"
  } else {
    return(list(predictions = q_hat, pars = gpd_pars, threshold = t_x0,
                lambda = lambda, max_weight = max_weight))
    # !!! do not return lambda when param_est == "Hill"
  }
}

validate_inputs <- function(object,  quantiles, threshold, newdata,
                            model_assessment, Y.test, out_of_bag,
                            lambda, max_weight){
  ## ... -> boolean
  ## check whether inputs are well-formed

  check_object(object)

  check_newdata_object(newdata, object)

  check_quantiles_thres(quantiles, threshold)

  check_model_assessment(model_assessment, newdata, Y.test)

  check_nonneg_numeric(lambda)

  check_nonneg_numeric(max_weight)

  return(TRUE)
}

check_nonneg_numeric <- function(x){
  ## numeric -> boolean
  ## check whether x is non-negative numeric of length 1

  arg <- deparse(substitute(x))

  if(length(x) != 1){
    stop(paste0(arg, " must be non-negative numeric of length 1"))
  } else if (x < 0 | !is.numeric(x)) {
    stop(paste0(arg, " must be non-negative numeric of length 1"))
  }

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

fit_conditional_gpd <- function(object, wi_x0, t_xi, lambda){
  ## quantile_forest numeric_matrix numeric_vector numeric -> matrix
  ## produce matrix with MLE GPD scale and shape parameter for each test point

  ntest <- nrow(wi_x0)
  Y <- object$Y.orig
  exc_idx = which(Y - t_xi > 0)
  exc_data = (Y - t_xi)[exc_idx]
  init_par <- ismev::gpd.fit(exc_data, 0, show=FALSE)$mle

  wi_x0 <- wi_x0[, exc_idx]
  EVT_par <- purrr::map_dfr(1:ntest, optim_wrap, init_par, weighted_llh,
                            exc_data, wi_x0, lambda, init_par[2])

  return(as.matrix(EVT_par))
}

fit_param_hill <- function(object, wi_x0, t_xi, t_x0, t_x0_2, threshold){
  ## quantile_forest numeric_matrix numeric_vector (3x) numeric -> matrix
  ## produce matrix with shape and scale param obtained with
  ## weighted Hill's estimator, for each test point.

  ntest <- nrow(wi_x0)
  n <- ncol(wi_x0)
  k <- floor(n * (1 - threshold))

  Y <- object$Y.orig
  exc_idx = which(Y - t_xi > 0)

  xi <- 1 / k * wi_x0[, exc_idx] %*% log(Y[exc_idx] / t_xi[exc_idx])
  sigma <- xi / (1 - 2 ^ (- xi)) * (t_x0 - t_x0_2)

  return(cbind(sigma, xi))
}

optim_wrap <- function(i, init_par, obj_fun, exc_data, wi_x0, lambda, xi_prior){

  curr_wi_x0 <- wi_x0[i, ]
  res <- stats::optim(par = init_par, fn = obj_fun, data = exc_data,
                      weights = curr_wi_x0, lambda = lambda,
                      xi_prior = xi_prior)$par
  names(res) <- c("sigma", "csi")
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

weighted_LLH <- function(par, data, weights, lambda, xi_prior) {
  ## numeric_vector numeric_matrix numeric_vector numeric numeric -> numeric
  ## returns the weighted penalized GPD log-likelihood

  sig = par[1] # sigma
  xi = par[2] # xi
  y = 1 + (xi/sig) * data
  if (min(sig) <= 0)
    nl = 10^6
  else {
    if (min(y) <= 0)
      nl = 10^6
    else {
      nl = sum(weights*(log(sig) + (1 + 1/xi)*log(y))) / length(weights) +
        lambda * (xi - xi_prior) ^ 2

    }
  }
  return(nl)
}

q_GPD <- function(p, p0, t_x0, sigma, xi){
  ## numeric(0, 1) numeric(0, 1) numeric_matrix numeric_vector
  ## numeric_vector -> numeric_vector
  ## produce the estimated extreme quantiles of GPD

  (((1-p)/(1-p0))^{-xi} - 1) * (sigma / xi) + t_x0
}

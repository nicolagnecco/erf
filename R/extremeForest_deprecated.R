#' Extreme predictions with a quantile forest
#'
#' Predicts high conditional quantiles of Y given X using a quantile forest
#' fitted using \code{\link[grf]{quantile_forest}}.
#'
#' @param object Quantile forest object. The trained forest obtained with
#'               \code{\link[grf]{quantile_forest}}.
#' @param quantiles Numeric vector (0, 1).
#'                  Extreme quantiles at which estimates are required.
#'                  Default is \code{quantiles = c(0.95, 0.99)}.
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
#' @param param_est Character. One of \code{"ML"} and \code{"Hill"}. Specifies the method to
#'                  fit the shape and scale parameter of the GDP distribution.
#'                  \code{"ML"} is the weighted maximum likelihood approach proposed by
#'                  \insertCite{merg2020;textual}{erf}.
#'                  \code{"Hill"} is the weighted Hill's estimator approach proposed
#'                  by \insertCite{deub2020;textual}{erf}.
#'                  Default is \code{"ML"}.
#' @param lambda Numeric (>= 0). Penalty coefficient for the \eqn{\xi}{csi} parameter
#'               in the weighted log-likelihood, i.e., when \code{param_est = "ML"}.
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
#' \item \code{plot} (if \code{model_assessment = TRUE}). QQ-plot for model assessment.
#' }
#'
#'
#' @references
#'  \insertAllCited{}
#'
#' @noRd
predict_erf <- function(object, quantiles = c(0.95, 0.99), threshold = 0.8,
                        newdata = NULL, model_assessment = FALSE,
                        Y.test = NULL, out_of_bag = FALSE,
                        param_est = c("ML", "Hill"), lambda = 0) {
  predict_erf_internal(
    object, quantiles, threshold,
    newdata, model_assessment, Y.test, out_of_bag,
    param_est, lambda
  )
}

predict_erf_internal <- function(object, quantiles = c(0.95, 0.99),
                                 threshold = 0.8,
                                 newdata = NULL, model_assessment = FALSE,
                                 Y.test = NULL, out_of_bag = FALSE,
                                 param_est = c("ML", "Hill"),
                                 lambda = 0,
                                 wi_x0 = NULL,
                                 t_xi = NULL, t_x0 = NULL, t_x0_2 = NULL) {

  ## same inputs as predict_erf + wi_x0, t_xi, t_x0, t_x0_2
  ##      -> same output as predict_erf
  ## same purpose as predict_erf

  ## !!! in the future,
  ## 1. remove args:
  ## - param_est = "Hill",
  ## - t_x0_2,
  ## 2. export args:
  ## - t_xi,
  ## - t_x0

  validate_inputs(
    object, quantiles, threshold, newdata, model_assessment,
    Y.test, out_of_bag, lambda
  )

  param_est <- match.arg(param_est)

  X0 <- set_test_observations(object, newdata)

  if (is.null(wi_x0)) {
    wi_x0 <- as.matrix(grf::get_forest_weights(object,
      newdata = X0,
      num.threads = NULL
    ))
  }

  if (is.null(t_xi)) {
    t_xi <- compute_thresholds(object,
      threshold = threshold,
      X = object$X.orig, out_of_bag = out_of_bag
    )
  }

  if (is.null(t_x0)) {
    t_x0 <- compute_thresholds(object, threshold = threshold, X = X0)
  }

  if (is.null(t_x0_2) & param_est == "Hill") {
    threshold2 <- 1 - 2 * (1 - threshold)
    t_x0_2 <- compute_thresholds(object, threshold = threshold2, X = X0)
  }

  if (param_est == "ML") {
    gpd_pars <- fit_conditional_gpd_deprecated(object, wi_x0, t_xi, lambda)
  } else if (param_est == "Hill") {
    gpd_pars <- fit_param_hill(object, wi_x0, t_xi, t_x0, t_x0_2, threshold)
  }

  q_hat <- compute_extreme_quantiles_deprecated(gpd_pars, t_x0, quantiles, threshold)

  if (model_assessment) {
    p <- compute_model_assessment_deprecated(t_x0, Y.test, gpd_pars)
    return(list(
      predictions = q_hat, pars = gpd_pars, threshold = t_x0,
      plot = p
    ))
  } else {
    return(list(predictions = q_hat, pars = gpd_pars, threshold = t_x0))
  }
}

#' Cross validation (CV) for Extreme Random Forest
#'
#' Runs repeated CV for Extreme Random Forest.
#'
#' @param X Numeric matrix. Matrix of predictors.
#' @param Y Numeric vector. Vector of responses.
#' @param t_xi Numeric vector. Vector of thresholds.
#' @param threshold Numeric (0, 1). Intermediate quantile used to compute
#'                  thresholds \eqn{t(x_i)}.
#' @param min.node.size Numeric vector. Grid of values of \code{min.node.size}
#'        to cross-validate.
#'        Default is \code{min.node.size = c(5)}.
#' @param lambda Numeric vector. Grid of values of \code{lambda}
#'        to cross-validate.
#'        Default is \code{lambda = 0}.
#' @param K Positive integer. Number of folds in CV.
#'        Default is \code{K = 5}.
#' @param n_rep Positive integer. Number of CV repetitions.
#'        Default is \code{n_rep = 1}.
#' @param args_grf List. List of named arguments for
#'        \code{\link[grf]{quantile_forest}}.
#'        Default is \code{args_grf = list()}.
#' @param args_erf List. List of named arguments for
#'        \code{\link[erf]{predict_erf}}.
#'        Default is \code{args_erf = list()}.
#' @param rng Numeric vector. Vector with
#'        \code{K * n_rep * length(min.node.size) + 1} seeds used to
#'        reproduce the random splits and fitting.
#'        Default is \code{rng = NULL}.
#' @param verbose (logical): whether to print cross validation logs. If \code{TRUE},
#'        they are printed to \code{log_file}
#'        Default is \code{verbose = FALSE}.
#' @param log_file (character): file where cross validation logs are saved.
#'        Default is \code{log_file = "./log.txt"}.
#'
#' @return Tibble. The columns are
#' \itemize{
#' \item \code{min.node.size} --- Numeric vector. Grid of values of \code{min.node.size}
#'        to cross-validate.
#'        \code{lambda} --- Numeric vector. Grid of values of \code{lambda}
#'        to cross-validate.
#' \item \code{cv_err} --- Numeric vector. Vector of cross validation errors.
#' \item \code{cv_se} --- Numeric vector. Vector of cross validation standard errors.
#' }
#'
#'
#' @references
#'  \insertAllCited{}
#'
#'
#'
#' @importFrom foreach foreach %dopar%
#' @importFrom magrittr %>%
#'
#' @noRd
erf_cv_deprecated <- function(X, Y, t_xi, threshold, min.node.size = 5, lambda = 0,
                   K = 5, n_rep = 1,
                   args_grf = list(), args_erf = list(),
                   rng = NULL, verbose = FALSE, log_file = "./log.txt") {
  if (is.null(rng)) {
    rng <- as.numeric(sample(1:1e6,
      size = n_rep *
        K * length(min.node.size) * length(lambda) + 1
    ))
  }

  check_rng(rng, n_rep, K, min.node.size, lambda)

  check_fn_params(grf::quantile_forest, lst = args_grf)
  check_fn_params(predict_erf, lst = args_erf)

  n <- nrow(X)

  folds <- create_folds(n, n_rep, K, seed = rng[[1]])

  grid <- expand.grid(
    n_rep = 1:n_rep, K_fold_out = 1:K,
    min.node.size = min.node.size,
    lambda = lambda
  ) %>%
    dplyr::bind_cols(tibble::tibble(rng = rng[-1])) %>%
    tibble::as_tibble() %>%
    dplyr::arrange(n_rep, K, min.node.size, lambda)

  iterations <- seq_len(nrow(grid))

  ll <- foreach(
    i = iterations, .combine = c,
    .options.future = list(scheduling = FALSE)
  ) %dopar% {
    grf_fit_fn <- purrr::partial(
      grf::quantile_forest,
      !!!args_grf
    )
    erf_predict_fn <- purrr::partial(
      predict_erf_internal,
      !!!args_erf
    )

    rng_curr <- grid$rng[[i]]
    n_rep <- grid$n_rep[i]
    K <- grid$K_fold_out[i]
    min.node.size <- grid$min.node.size[i]
    lambda <- grid$lambda[i]

    if (verbose) {
      cat("n_rep =", n_rep, "--- K =", K,
        "--- min.node.size =", min.node.size,
        "--- lambda =", lambda, "\n",
        file = log_file, append = TRUE
      )
    }

    dat <- split_data_deprecated(X, Y, t_xi, folds[[n_rep]], K)

    # rngtools::setRNG(rng_curr)

    fit.grf <- grf_fit_fn(
      X = dat$train$X, dat$train$Y,
      min.node.size = min.node.size
    )

    exc_id <- dat$valid$Y > dat$valid$t_xi

    gpd_pars <- erf_predict_fn(fit.grf,
      newdata = dat$valid$X[exc_id, ],
      t_xi = dat$train$t_xi,
      t_x0 = dat$valid$t_xi[exc_id],
      lambda = lambda
    )$pars


    n_valid <- length(dat$valid$Y)

    evaluate_deviance_deprecated(
      gpd_pars,
      dat$valid$Y[exc_id],
      dat$valid$t_xi[exc_id]
    ) / (n_valid * (1 - threshold))
  }

  # if (any(!isnt_out_mad(ll))){
  #   msg <- paste0("Some repetitions produced unreliable ",
  #                 "cross validation errors and were discarded.")
  #   warning(msg)
  #   if (verbose){cat(msg, "\n", file = log_file, append = TRUE)}
  # }

  res <- dplyr::bind_cols(grid, tibble::tibble(cv_K_fold_out = ll)) %>%
    # remove_outliers_cv() %>%
    dplyr::group_by(min.node.size, lambda) %>%
    dplyr::summarise(
      cv_err = mean(cv_K_fold_out),
      cv_se = 1 / sqrt(K) * stats::sd(cv_K_fold_out)
    )



  return(res)
}

remove_outliers_cv <- function(tbl) {
  ## tibble function -> tibble
  ## remove all cross validation repetitions where some outliers occured

  # identify the repetition with outliers
  cond <- isnt_out_mad(tbl$cv_K_fold_out)

  # identify the fold associated with outliers
  if (any(!cond)) {
    Ks <- unique(tbl$K_fold_out[!cond])
    n_reps <- unique(tbl$n_rep[!cond])

    return(tbl %>%
      dplyr::filter(!((K_fold_out %in% Ks) & (n_rep %in% n_reps))))
  } else {
    return(tbl)
  }
}


isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  ## numeric_vector numeric logical -> logical_vector
  ## produce a logical vector with TRUE if the corresponding element is within
  ## thres standard deviations from its mean

  abs(x - mean(x, na.rm = na.rm)) <= thres * stats::sd(x, na.rm = na.rm)
}


isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  ## numeric_vector numeric logical -> logical_vector
  ## produce a logical vector with TRUE if the corresponding element is within
  ## thres mad from its median
  abs(x - stats::median(x, na.rm = na.rm)) <= thres * stats::mad(x, na.rm = na.rm)
}


evaluate_deviance_deprecated <- function(gpd_pars, Y, t_xi) {
  ## numeric_matrix numeric_vector (2x) -> numeric
  ## evaluate the GDP deviance

  exc <- Y - t_xi
  data <- exc[exc > 0]
  sig <- gpd_pars[exc > 0, 1]
  xi <- gpd_pars[exc > 0, 2]
  y <- 1 + (xi / sig) * data

  if (min(sig) <= 0) {
    return(10^6)
  } else {
    if (min(y) <= 0) {
      return(10^6)
    } else {
      return(sum(log(sig) + (1 + 1 / xi) * log(y)))
    }
  }
}

split_data_deprecated <- function(X, Y, t_xi, fold, K) {
  ## numeric_matrix numeric_vector (2x) list integer -> list
  ## produce a list made of:
  ## train (contains all rows not in fold[[K]])
  ## - X, Y, t_xi
  ## valid (contains all rows in fold[[K]])
  ## - X, Y, t_xi

  ll <- list()
  ll$train$X <- X[-fold[[K]], ]
  ll$train$Y <- Y[-fold[[K]]]
  ll$train$t_xi <- t_xi[-fold[[K]]]

  ll$valid$X <- X[fold[[K]], ]
  ll$valid$Y <- Y[fold[[K]]]
  ll$valid$t_xi <- t_xi[fold[[K]]]

  return(ll)
}



validate_inputs <- function(object, quantiles, threshold, newdata,
                            model_assessment, Y.test, out_of_bag,
                            lambda) {
  ## ... -> boolean
  ## check whether inputs are well-formed

  check_object(object)

  check_newdata_object(newdata, object)

  check_quantiles_thres(quantiles, threshold)

  check_model_assessment(model_assessment, newdata, Y.test)

  check_nonneg_numeric(lambda)

  return(TRUE)
}

check_rng <- function(rng, K, n_rep, min.node.size, lambda) {
  ## numeric_vector integer integer numeric_vector (2x) -> boolean
  ## check whether rng has the right size

  arg <- deparse(substitute(rng))

  correct_size <- K * n_rep * length(min.node.size) * length(lambda) + 1
  cond <- (length(rng) == correct_size)

  if (!cond) {
    stop(paste0(arg, " must contain ", correct_size, " elements"))
  }

  return(TRUE)
}


check_nonneg_numeric <- function(x) {
  ## numeric -> boolean
  ## check whether x is non-negative numeric of length 1

  arg <- deparse(substitute(x))

  if (length(x) != 1) {
    stop(paste0(arg, " must be non-negative numeric of length 1"))
  } else if (x < 0 | !is.numeric(x)) {
    stop(paste0(arg, " must be non-negative numeric of length 1"))
  }

  return(TRUE)
}

check_object <- function(object) {
  ## quantile_forest -> boolean
  ## check whether object is of class "quantile_forest"

  if (class(object)[1] != "quantile_forest") {
    stop("object must be of class 'quantile_forest'")
  }

  return(TRUE)
}

check_newdata_object <- function(newdata, object) {
  ## numeric_matrix, quantile_forest -> boolean
  ## check whether newdata and object are well-formed

  if (!is.null(newdata)) {
    if (!("matrix" %in% class(newdata))) {
      stop("newdata must be of class 'matrix'")
    }

    if (ncol(newdata) != ncol(object$X.orig)) {
      stop("newdata must have the same number of columns as the training matrix")
    }
  }

  return(TRUE)
}

check_quantiles_thres <- function(quantiles, threshold) {
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

  if (any(quantiles < threshold)) {
    stop("all quantiles must be larger than threshold")
  }

  return(TRUE)
}

check_model_assessment <- function(model_assessment, newdata, Y.test) {
  ## boolean, numeric_matrix, numeric_vector -> boolean
  ## check whether inputs are well formed

  if (model_assessment) {
    if (is.null(newdata) || is.null(Y.test)) {
      stop("newdata and Y.test must be supplied when model_assessment = TRUE")
    }
  }

  if (!is.null(newdata) && !is.null(Y.test)) {
    valid.classes <- c("matrix", "numeric")

    if (!class(Y.test) %in% valid.classes) {
      stop(paste("Y.test must be one of the following classes:",
        paste(valid.classes, collapse = ", "),
        sep = " "
      ))
    }

    n_ytest <- if (class(Y.test) == "numeric") {
      length(Y.test)
    } else {
      nrow(Y.test)
    }

    if (n_ytest != nrow(newdata)) {
      stop("newdata and Y.test must have the same number of observations")
    }
  }

  return(TRUE)
}

set_test_observations <- function(object, newdata) {
  ## quantile_forest numeric_matrix -> numeric_matrix
  ## set test observation matrix

  if (is.null(newdata)) {
    X0 <- object$X.orig
  } else {
    X0 <- newdata
  }

  return(X0)
}

compute_thresholds <- function(object, threshold, X, out_of_bag = FALSE) {
  ## quantile_forest numeric(0, 1) numeric_matrix boolean -> numeric_matrix
  ## compute conditional quantile based on quantile_forest object

  if (out_of_bag) {
    q_hat <- unlist(stats::predict(object, quantiles = threshold))
  } else {
    q_hat <- unlist(stats::predict(object, newdata = X, quantiles = threshold))
  }

  return(q_hat)
}

fit_conditional_gpd_deprecated <- function(object, wi_x0, t_xi, lambda) {
  ## quantile_forest numeric_matrix numeric_vector numeric -> matrix
  ## produce matrix with MLE GPD scale and shape parameter for each test point

  ntest <- nrow(wi_x0)
  Y <- object$Y.orig
  exc_idx <- which(Y - t_xi > 0)
  exc_data <- (Y - t_xi)[exc_idx]
  init_par <- ismev::gpd.fit(exc_data, 0, show = FALSE)$mle

  wi_x0 <- wi_x0[, exc_idx]
  EVT_par <- purrr::map_dfr(
    1:ntest, optim_wrap, init_par, weighted_llh,
    exc_data, wi_x0, lambda, init_par[2]
  )

  return(as.matrix(EVT_par))
}

fit_param_hill <- function(object, wi_x0, t_xi, t_x0, t_x0_2, threshold) {
  ## quantile_forest numeric_matrix numeric_vector (3x) numeric -> matrix
  ## produce matrix with shape and scale param obtained with
  ## weighted Hill's estimator, for each test point.

  ntest <- nrow(wi_x0)
  n <- ncol(wi_x0)
  k <- floor(n * (1 - threshold))

  Y <- object$Y.orig
  exc_idx <- which(Y - t_xi > 0)

  xi <- n / k * wi_x0[, exc_idx] %*% log(Y[exc_idx] / t_xi[exc_idx])
  sigma <- xi / (1 - 2^(-xi)) * (t_x0 - t_x0_2)

  return(cbind(sigma, xi))
}

optim_wrap <- function(i, init_par, obj_fun, exc_data, wi_x0, lambda, xi_prior) {
  curr_wi_x0 <- wi_x0[i, ]
  res <- stats::optim(
    par = init_par, fn = obj_fun, data = exc_data,
    weights = curr_wi_x0, lambda = lambda,
    xi_prior = xi_prior
  )$par
  names(res) <- c("sigma", "csi")
  return(res)
}

compute_extreme_quantiles_deprecated <- function(gpd_pars, t_x0, quantiles, threshold) {
  ## numeric_matrix numeric_vector numeric_vector(0, 1)
  ## numeric(0, 1) -> numeric_matrix
  ## produce matrix with estimated extremes quantiles. The value at (i, j) gives
  ## the estimated quantile j for test sample i

  res <- matrix(nrow = length(t_x0), ncol = length(quantiles))

  for (j in seq_along(quantiles)) {
    res[, j] <- q_GPD(
      p = quantiles[j], p0 = threshold, t_x0 = t_x0,
      sigma = gpd_pars[, 1], xi = gpd_pars[, 2]
    )
  }

  colnames(res) <- paste("quantile = ", quantiles)

  return(res)
}

compute_model_assessment_deprecated <- function(t_x0, Y.test, gpd_pars) {
  ## numeric_vector numeric_vector numeric_matrix -> plot
  ## produce QQ-plot of standardized estimated quantiles vs exponential data

  Y <- Y.test
  exc_idx <- which(Y - t_x0 > 0)
  exc_data <- (Y - t_x0)[exc_idx]
  xi <- gpd_pars[exc_idx, 2]
  sigma <- gpd_pars[exc_idx, 1]

  pseudo_obs <- compute_pseudo_observations(exc_data, sigma, xi)
  n <- length(pseudo_obs)

  observed_quantiles <- sort(pseudo_obs)
  theoretical_quantiles <- stats::qexp((1:n) / (n + 1))

  plot_model_assessment(theoretical_quantiles, observed_quantiles)
}

plot_model_assessment <- function(x, y) {
  ## numeric_vector numeric_vector -> plot
  ## produce QQ-plot

  my_colors <- c("#0072B2", "#D55E00", "#CC79A7")

  dat_plot <- data.frame(
    theoretical_quantiles = x,
    observed_quantiles = y
  )

  dat_random <- sample_exponentials(dat_plot)

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = dat_random,
      ggplot2::aes_string(
        x = "theoretical_quantiles",
        y = "random_exp",
        group = "rep"
      ),
      alpha = 0.2, col = "#D55E00"
    ) +
    ggplot2::geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", size = 1, col = "#0072B2"
    ) +
    ggplot2::geom_point(
      data = dat_plot,
      ggplot2::aes_string(
        x = "theoretical_quantiles",
        y = "observed_quantiles"
      ),
      size = 1
    ) +
    ggplot2::ggtitle("QQ-plot") +
    ggplot2::xlab("theoretical quantiles") +
    ggplot2::ylab("observed quantiles") +
    ggplot2::theme_bw()
}

sample_exponentials <- function(dat_plot) {
  ## data_frame -> data_frame
  ## produce data_frame with 100 randomly sampled exponential observations

  n <- nrow(dat_plot)

  dat <- data.frame(
    random_exp = double(), theoretical_quantiles = double(),
    rep = double()
  )
  for (i in 1:100) {
    random_exp <- sort(stats::rexp(n = n))
    theoretical_quantiles <- sort(dat_plot$theoretical_quantiles)

    dat <- rbind(
      dat,
      data.frame(random_exp, theoretical_quantiles,
        rep = rep(i, n)
      )
    )
  }

  return(dat)
}

weighted_LLH <- function(par, data, weights, lambda, xi_prior) {
  ## numeric_vector numeric_matrix numeric_vector numeric numeric -> numeric
  ## returns the weighted penalized GPD log-likelihood

  sig <- par[1] # sigma
  xi <- par[2] # xi
  y <- 1 + (xi / sig) * data
  if (min(sig) <= 0) {
    nl <- 10^6
  } else {
    if (min(y) <= 0) {
      nl <- 10^6
    } else {
      nl <- sum(weights * (log(sig) + (1 + 1 / xi) * log(y))) / length(weights) +
        lambda * (xi - xi_prior)^2
    }
  }
  return(nl)
}

extract_fn_params <- function(fn, lst) {
  ## function list -> list
  ## extracts from named lst the elements which are valid arguments to fn

  required_args <- formals(fn)
  lst[names(lst) %in% names(required_args)]
}

check_fn_params <- function(fn, lst) {
  ## function list -> boolean
  ## produce true if elements lst are valid arguments for fn

  arg1 <- deparse(substitute(fn))
  arg2 <- deparse(substitute(lst))

  required_args <- formals(fn)
  cond <- names(lst) %in% names(required_args)

  if (!all(cond)) {
    stop(paste0(
      arg2, " contains the wrong arguments for ", arg1,
      ". These are: ", names(lst)[!cond]
    ))
  } else {
    return(TRUE)
  }
}


predict_erf_internal2 <- function(object, quantiles = c(0.95, 0.99),
                                  threshold = 0.8,
                                  newdata = NULL, model_assessment = FALSE,
                                  Y.test = NULL, out_of_bag = FALSE,
                                  param_est = c("ML", "Hill"),
                                  lambda = 0,
                                  wi_x0 = NULL,
                                  t_xi = NULL, t_x0 = NULL, t_x0_2 = NULL) {

  ## same inputs as predict_erf + wi_x0, t_xi, t_x0, t_x0_2
  ##      -> same output as predict_erf
  ## same purpose as predict_erf

  ## !!! in the future,
  ## 1. remove args:
  ## - param_est = "Hill",
  ## - t_x0_2,
  ## 2. export args:
  ## - t_xi,
  ## - t_x0

  validate_inputs(
    object, quantiles, threshold, newdata, model_assessment,
    Y.test, out_of_bag, lambda
  )

  param_est <- match.arg(param_est)

  X0 <- set_test_observations(object, newdata)

  if (is.null(wi_x0)) {
    wi_x0 <- as.matrix(grf::get_forest_weights(object,
      newdata = X0,
      num.threads = NULL
    ))
  }

  if (is.null(t_xi)) {
    t_xi <- compute_thresholds(object,
      threshold = threshold,
      X = object$X.orig, out_of_bag = out_of_bag
    )
  }

  if (is.null(t_x0)) {
    t_x0 <- compute_thresholds(object, threshold = threshold, X = X0)
  }

  if (is.null(t_x0_2) & param_est == "Hill") {
    threshold2 <- 1 - 2 * (1 - threshold)
    t_x0_2 <- compute_thresholds(object, threshold = threshold2, X = X0)
  }

  if (param_est == "ML") {
    gpd_pars <- fit_conditional_gpd2(object, wi_x0, t_xi, lambda)
  } else if (param_est == "Hill") {
    gpd_pars <- fit_param_hill(object, wi_x0, t_xi, t_x0, t_x0_2, threshold)
  }

  q_hat <- compute_extreme_quantiles_deprecated(gpd_pars, t_x0, quantiles, threshold)

  if (model_assessment) {
    p <- compute_model_assessment_deprecated(t_x0, Y.test, gpd_pars)
    return(list(
      predictions = q_hat, pars = gpd_pars, threshold = t_x0,
      plot = p
    ))
  } else {
    return(list(predictions = q_hat, pars = gpd_pars, threshold = t_x0))
  }
}

fit_conditional_gpd2 <- function(object, wi_x0, t_xi, lambda) {
  ## quantile_forest numeric_matrix numeric_vector numeric -> matrix
  ## produce matrix with MLE GPD scale and shape parameter for each test point

  ntest <- nrow(wi_x0)
  Y <- object$Y.orig
  exc_idx <- which(Y - t_xi > 0)
  exc_data <- (Y - t_xi)
  init_par <- ismev::gpd.fit(exc_data, 0, show = FALSE)$mle

  wi_x0 <- wi_x0
  EVT_par <- purrr::map_dfr(
    1:ntest, optim_wrap2, init_par, weighted_llh,
    exc_data, wi_x0, lambda, init_par[2], exc_idx
  )

  return(as.matrix(EVT_par))
}

optim_wrap2 <- function(i, init_par, obj_fun, exc_data, wi_x0, lambda, xi_prior,
                        exc_idx) {
  exclude_obs <- c(i, (1:length(exc_data))[-exc_idx])
  exc_data <- exc_data[-exclude_obs]

  curr_wi_x0 <- wi_x0[i, -exclude_obs]
  res <- stats::optim(
    par = init_par, fn = obj_fun, data = exc_data,
    weights = curr_wi_x0, lambda = lambda,
    xi_prior = xi_prior
  )$par
  names(res) <- c("sigma", "csi")
  return(res)
}

create_folds <- function(n, n_rep, K, seed){
  ## integer (4x) -> list
  ## produce a list with n_rep splits for K-fold CV

  rows_id <- 1:n

  rngtools::setRNG(seed)
  lapply(X = rep(1, n_rep), FUN = function(x){
    chunk(sample(rows_id), K)
  })
}

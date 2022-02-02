#' Cross-validation for an extremal random forest (ERF)
#'
#'
#' Internal function to fit an extremal random forest with cross-validation ...
#' !!! write description
#'
#'
#' !!! write details
#'
#' @inheritParams erf_cv
#' @inheritParams erf
#'
#'
#' @inherit erf_cv return
#'
#'
#' @examples
#' "!!! add examples"
#'
#' @importFrom rlang .data
#'
#' @noRd
fit_erf_cv <- function(X,
                       Y,
                       min.node.size = c(5, 40, 100),
                       lambda = c(0, 0.001, 0.01),
                       intermediate_estimator = c("grf", "neural_nets"),
                       intermediate_quantile = 0.8,
                       nfolds = 5, nreps = 3, seed = NULL){

  # fit intermediate quantile estimator
  intermediate_threshold <- fit_intermediate_threshold(
    X, Y,
    intermediate_estimator)

  # predict intermediate_threshold Q_X
  Q_X <- predict_intermediate_quantile(
    intermediate_threshold = intermediate_threshold,
    intermediate_quantile = intermediate_quantile
  )

  # create splits
  splits <- get_repeated_k_folds(nrow(X), nfolds, nreps, seed)

  # create parameter grid
  params <- get_param_grid(min.node.size, lambda)

  # create full grid with parameters and splits
  full_grid <- tidyr::crossing(splits, params)
  fun_args <- full_grid %>%
    dplyr::select(- .data$rep_id, - .data$fold_id)

  # partialise fit_and_score_erf_lw
  fit_and_score_partial <- purrr::partial(
    fit_and_score_erf_lw,
    X = X, Y = Y,
    Q_X = Q_X,
    intermediate_quantile = intermediate_quantile
  )

  # sweep through folds and parameters
  scores <- full_grid %>%
    dplyr::mutate(cv_err = purrr::pmap_dbl(fun_args, fit_and_score_partial))

  # compute optimal parameters
  opt_params <- scores %>%
    dplyr::group_by(.data$min.node.size, .data$lambda) %>%
    dplyr::summarise(cvm = mean(.data$cv_err)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$cvm == min(.data$cvm))

  # refit `erf` on full dataset
  fit.erf <- fit_erf(X, Y, opt_params$min.node.size, opt_params$lambda,
          intermediate_estimator, intermediate_quantile)

  # return `erf_cv`
  structure(
    list(scores = scores, fit.erf = fit.erf),
    class = "erf_cv"
  )

}

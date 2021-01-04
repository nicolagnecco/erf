#' Fit Extreme Random Forest
#'
#' Fit an extreme random forest (\code{erf}) object given a set of predictors
#' \code{X}, response \code{Y} and minimum node size value
#' (\code{min.node.size}) for the underlying quantile random forest.
#'
#' The function fits an \code{erf} object as described by
#' \insertCite{merg2020;textual}{erf}.
#' In particular, it fits a \code{\link[grf]{quantile_forest}} (\code{model_1})
#' with given \code{min.node.size} that is used to obtain the weights,
#' and it fits a secondary \code{\link[grf]{quantile_forest}} (\code{model_2})
#' with default parameters that is used to obtain the intermediate threshold.
#' It returns an \code{erf} object (see documentation !!!).
#'
#' It is possible to customize the arguments for \code{model_1}, and choose
#' a different \code{model_2} with corresponding arguments by calling
#' \code{fit_erf_devoloper} (see documentiation !!!).
#'
#' @param X Numeric matrix. Matrix of predictors.
#' @param Y Numeric vector. Vector of response.
#' @param min.node.size Integer. A target for the minimum number of observations in
#'        each tree leaf for the fitted \code{model_1}.
#'        For further details, refer to \code{\link[grf]{quantile_forest}}.
#'        Default is 40.
#'
#' @return A fitted \code{erf} object (see documentation!!!).
#'
#' @references
#'  \insertAllCited{}
#'
#' @export
fit_erf <- function(X, Y, min.node.size = 40){

  args_model_1 <- list(X = X, Y = Y, min.node.size = min.node.size)
  model_2 <- grf::quantile_forest
  args_model_2 <- list(X = X, Y = Y)

  fit_erf_developer(args_model_1, model_2, args_model_2)

}

#' Fit Extreme Random Forest (developer version)
#'
#' This is an advanced interface to fit an \code{erf} object.
#'
#' This function allows the user to customize the arguments for the
#' \code{\link[grf]{quantile_forest}}, i.e., \code{args_model_1},
#' and to choose the algorithm to fit the second model \code{model_2},
#' with related custom arguments \code{args_model_2}.
#'
#' Recall that \code{\link[grf]{quantile_forest}} is used by \code{erf} to
#' obtain the weights used in the weighted log-likelihood.
#' At the same time, \code{model_2} is used to predict the intermediate
#' threshold.
#'
#' For further details see \insertCite{merg2020;textual}{erf}.
#'
#' @param args_model_1 Named list. List of named arguments for
#' \code{\link[grf]{quantile_forest}}.
#' @param model_2 Function. A function used to fit the intermediate
#'
#' @return A fitted \code{erf} object (see documentation!!!).
#'
#' @references
#'  \insertAllCited{}
#'
#' @export
fit_erf_developer <- function(args_model_1, model_2, args_model_2){
  # check args_model_1
  validate_fn_args(fn = grf::quantile_forest, lst = args_model_1)

  # check model_2
  validate_fn(model_2)

  # check args_model_2
  validate_fn_args(fn = model_2, lst = args_model_2)
  # ...

  # return erf object
  new_erf(...) # !!! create constructor
}



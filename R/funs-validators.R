validate_intermediate_quantile <- function(quantile) {
  ## numeric -> invisible(numeric)
  ## produce quantile if the parameters are well formed, throws error if not

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  invisible(quantile)
}

validate_data <- function(X, Y) {
  ## numeric_matrix numeric_vector -> invisible(list)
  ## produce list(X, Y) if the parameters are well formed, throws error if not

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  invisible(list(X, Y))
}


validate_params <- function(min.node.size, lambda, intermediate_quantile) {
  ## numeric numeric -> list
  ## produce list(min.node.size, lambda) if the parameters are well formed,
  ## throws error if not

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  invisible(list(min.node.size, lambda))
}


validate_intermediate_estimator <- function(estimator
                                            = c("grf", "neural_nets")) {
  ## intermediate_estimator -> intermediate_estimator
  ## produce intermediate_estimator if it is well formed, throws error if not
  tryCatch(
    error = function(cnd) {
      abort_wrong_estimator(cnd$message)
    },
    invisible(match.arg(estimator))
  )
}


validate_erf <- function(ef) {
  ## erf -> erf
  ## produce ef if it is well formed, throws error if not

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  return(ef)
}

validate_erf_cv <- function(erf_cv){
  ## erf_cv -> erf_cv
  ## produce erf_cv if it is well formed, throws error if not

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  return(erf_cv)

}


validate_newdata_X <- function(newdata, ef, newdata_char = "newdata") {
  ## numeric_matrix erf  character -> numeric_matrix
  ## produce newdata if it is well formed, throws an error if not

  # if NULL exit
  if (is.null(newdata)) {
    return(invisible(newdata))
  }

  # check it is tabular
  if (length(dim(newdata)) != 2){
    abort_wrong_dimension(newdata_char, must = "have 2 dimensions",
                       not = as.character(length(dim(newdata))))
  }

  # check it has at least 1 row
  if (nrow(newdata) == 0) {
    abort_zero_rows(arg = newdata_char)
  }

  # check it has right # of columns
  p <- ncol(newdata)
  p_expect <- ncol(ef$quantile_forest$X.orig)

  if (p != p_expect) {
    abort_wrong_columns(arg = newdata_char, must_have = as.character(p_expect),
                        columns_not = as.character(p))
  }

  invisible(newdata)

}

validate_newdata_Y <- function(newdata, ef, newdata_char = "newdata_Y") {
  ## numeric_matrix erf character -> numeric_matrix
  ## produce newdata if it is well formed, throws an error if not

  # if NULL exit
  if (is.null(newdata)) {
    return(invisible(newdata))
  }

  # check it is *not* tabular
  if (length(dim(newdata)) != 0){
    abort_wrong_dimension(newdata_char, must = "be a vector with 0 dimensions",
                          not = as.character(length(dim(newdata))))
  }

  # check it has at least 1 row
  if (length(newdata) == 0) {
    abort_zero_rows(arg = newdata_char)
  }

  invisible(newdata)

}

validate_newdata_X_and_Y <- function(newdata_X, newdata_Y, ef,
                                     newdata_X_char = "newdata_X",
                                     newdata_Y_char = "newdata_Y") {
  ## numeric_matrix numeric_vector erf character character -> list
  ## produces a list with the correct pair of newdata_X and newdata_Y

  # check newdata_X is well-formed
  validate_newdata_X(newdata_X, ef, newdata_char = newdata_X_char)

  # check newdata_Y is well-formed
  validate_newdata_Y(newdata_Y, ef, newdata_char = newdata_Y_char)


  # combine newdata_X and newdata_Y
  if (is.null(newdata_X) & is.null(newdata_Y)) {
    return(
      list(newdata_X = NULL,
           newdata_Y = ef$quantile_forest$Y.orig)
    )

  } else if (!is.null(newdata_X) & !is.null(newdata_Y)) {
    # check newdata_X and newdata_Y have same # of observations
    if (nrow(newdata_X) != length(newdata_Y)) {
      abort_different_n_observations(newdata_X_char, newdata_Y_char)
    }

    return(
      list(newdata_X = newdata_X, newdata_Y = newdata_Y)
    )

  } else {
    abort_both_null_or_viceversa(newdata_X_char, newdata_Y_char)

  }
}

validate_quantiles <- function(quantiles, ef) {
  ## numeric_vector erf -> numeric_vector
  ## produce quantiles if they are well formed, throws error if not

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct
  invisible(quantiles)
}


has_method <- function(object, generic) {
  ## object generic -> boolean
  ## produces true if `object` has method `generic`, false otherwise

  ch <- deparse(substitute(generic))

  any(grepl(
    ch,
    sapply(
      class(object),
      function(cl) {
        utils::methods("class" = cl)
      }
    )
  ))
}

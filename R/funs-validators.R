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


validate_newdata <- function(newdata, ef) {
  ## numeric_matrix erf -> numeric_matrix
  ## produce newdata if it is well formed, throws an error if not

  # if NULL exit
  if (is.null(newdata)) {
    return(invisible(newdata))
  }

  # check it is tabular
  if (length(dim(newdata)) != 2){
    abort_wrong_dimension("newdata", must = "have 2 dimensions",
                       not = as.character(length(dim)))
  }

  # check it has at least 1 row
  if (nrow(newdata) == 0) {
    abort_zero_rows(arg = "newdata")
  }

  # check it has right # of columns
  p <- ncol(newdata)
  p_expect <- ncol(ef$quantile_forest$X.orig)

  if (p != p_expect) {
    abort_wrong_columns(arg = "newdata", must_have = as.character(p_expect),
                        columns_not = as.character(p))
  }

  invisible(newdata)

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

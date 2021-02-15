validate_data <- function(X, Y) {
  ## numeric_matrix numeric_vector -> invisible(list)
  ## checks whether the given data are well formed, throws error if not

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  invisible(list(X, Y))
}


validate_params <- function(min.node.size, lambda) {
  ## numeric numeric -> invisible(list)
  ## checks whether the given parameters are well formed, throws error if not

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  invisible(list(min.node.size, lambda))
}


# !!! change
validate_intermediate_estimator <- function(estimator, X) {
  ## intermdiate_estimator numeric_matrix -> intermediate_estimator
  ## returns estimator if it is well formed, throws error if not

  # if not NULL inspect
  if (!is.null(estimator)) {

    # check whether estimator has method predict
    if (!has_method(estimator, predict)) {
      stop("error", call. = FALSE)
    }

    # check whether predict(estimator, X) runs
    tryCatch(
      error = function(cnd) {
        abort_predict_on_fail(
          "intermediate_estimator",
          class(estimator)[1],
          "X",
          cnd$message
        )
      },
      predict(estimator, X)
    )
  }

  invisible(estimator)
}


validate_extreme_forest <- function(ef) {
  ## extreme_forest -> extreme_forest
  ## returns ef if it is well formed, throws error if not

  # !!!
  # examples
  # template / inventory
  # body
  # run test and debug until correct

  return(ef)
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
        methods("class" = cl)
      }
    )
  ))
}


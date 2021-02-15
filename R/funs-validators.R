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


validate_params <- function(min.node.size, lambda) {
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
    error = function(cnd){
      abort_wrong_estimator(cnd$message)
    },
    invisible(match.arg(estimator))
  )
}


validate_extreme_forest <- function(ef) {
  ## extreme_forest -> extreme_forest
  ## produce ef if it is well formed, throws error if not

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


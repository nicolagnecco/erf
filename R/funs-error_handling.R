warning_obs_outside_support <- function(idx) {
  ## numeric_vector -> warning_condition
  ## produce warning condition

  full_msg <- c(glue::glue("Problem with support of the data."),
                glue::glue("Observation `{idx}` is not plotted because it exceeds its upper end point."))

  names(full_msg) <- rep("i", length(full_msg))
  names(full_msg)[1] <- ""

  rlang::warn(
    message = full_msg,
    class = "warn_outside_support"
  )

}

abort_different_n_observations <- function(X, Y) {
  ## character character -> error_condition
  ## produce error condition


  full_msg <- c(glue::glue("Problem with `{X}` and `{Y}`."),
                x = glue::glue("`{X}` and `{Y}` must have the same number of observations."))

  rlang::abort(
    message = full_msg,
    class = "error_different_n_observations"
  )


}

abort_both_null_or_viceversa <- function(X, Y) {
  ## character character -> error_condition
  ## produce error condition

  full_msg <- c(glue::glue("Problem with `{X}` and `{Y}`."),
                x = glue::glue("`{X}` and `{Y}` must be both `NULL` or both not `NULL`."))

  rlang::abort(
    message = full_msg,
    class = "error_both_null"
  )

}

abort_wrong_dimension <- function(arg, must, not){
  ## character character character -> error_condition
  ## produce error condition

  full_msg <- c(glue::glue("Problem with the argument `{arg}`."),
                x = glue::glue("`{arg}` must {must}; not {not}."))

  rlang::abort(
    message = full_msg,
    class = "error_wrong_dimension"
  )

}

abort_zero_rows <- function(arg) {
  ## character -> error_condition
  ## produce error condition

  full_msg <- c(glue::glue("Problem with the argument `{arg}`."),
                x = glue::glue("`{arg}` must have at least 1 row."))

  rlang::abort(
    message = full_msg,
    class = "error_zero_rows"
  )
}

abort_wrong_columns <- function(arg, must_have, columns_not){
  ## character character character -> error_condition
  ## produce error condition

  full_msg <- c(glue::glue("Problem with the argument `{arg}`."),
                x = glue::glue("`{arg}` must have {must_have} columns; not {columns_not}."))

  rlang::abort(
    message = full_msg,
    class = "error_wrong_columns"
  )

}

abort_not_implemented <- function(type, name, fun_name = character(0)) {
  ## character character character -> error_condition
  ## produce error condition

  error_msg <- glue::glue("The {type} `{name}` is not implemented")

  if (rlang::is_empty(fun_name)){
    error_msg <- glue::glue(error_msg, ".")
  } else {
    error_msg <- glue::glue(error_msg, " in the function {fun_name}.")
  }

  full_msg <- c(glue::glue("Not-implemented problem."),
    x = error_msg
  )

  rlang::abort(
    message = full_msg,
    class = "error_not_implemented"
  )
}

abort_wrong_estimator <- function(chr) {
  ## character -> error_condition
  ## produce error condition

  full_msg <- c(glue::glue("Wrong `intermediate_estimator` supplied."),
    x = chr
  )

  rlang::abort(
    message = full_msg
  )
}

abort_predict_on_fail <- function(estimator, class, X, msg) {
  ## character character character character -> error_condition
  ## produce error condition

  full_msg <-
    c(glue::glue("Problem when calling `predict({estimator}, {X})`."),
      x = glue::glue("Message: ", msg),
      i = glue::glue("Can you run `predict({estimator}, {X})`?"),
      i = glue::glue("Does `{X}` have the correct data type? Check with `?predict.{class}`.")
    )

  rlang::abort(
    message = full_msg,
    class = "error_predict_fails"
  )
}


# examples
abort_bad_argument <- function(arg, must, not = NULL) {
  ## character character character|NULL -> error_condition
  ## produce error condition

  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  } else {
    msg <- glue::glue("{msg}.")
  }

  msg <- c(glue::glue("Problem with the argument `{arg}`."),
    x = msg
  )

  rlang::abort(
    message = msg,
    class = "error_bad_argument"
  )
}

my_log <- function(x, base = exp(1)) {
  if (!is.numeric(x)) {
    abort_bad_argument("x", must = "be numeric")
  }
  if (!is.numeric(base)) {
    abort_bad_argument("base", must = "be numeric", not = base)
  }

  base::log(x, base = base)
}

# my_log(2, base = letters)


my_f <- function(x = 1) {
  match.call()[[1]]
}

# a <- my_f(x = 2)

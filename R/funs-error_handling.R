abort_wrong_estimator <- function(cnd){
  ## error_condition -> error_condition
  ## produce error condition

  full_msg <- c(glue::glue("Wrong `intermediate_estimator` supplied."),
                x = cnd$message)

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

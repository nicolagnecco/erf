abort_bad_argument <- function(arg, must, not = NULL) {
  ## character character character|NULL -> error condition
  ## produce error condition

  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  } else {
    msg <- glue::glue("{msg}.")
  }

  msg <- c(glue::glue("Problem with the argument `{arg}`."),
           x = msg)

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

my_log(2, base = letters)


my_f <- function(x = 1){
  match.call()[[1]]
}

a <- my_f(x = 2)

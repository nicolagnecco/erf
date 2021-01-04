validate_fn <- function(fn){
  ## function -> boolean
  ## produce TRUE if fn is a function

  arg <- deparse(substitute(fn))

  if (!(is.function(fn))){
    stop(paste0(fn, " is not a function."))
  }

  return(TRUE)
}


validate_X <- function(X){
  ## matrix -> boolean
  ## produce true if X is a numeric matrix with no missing values

  if (any(is.na(X))){
    stop("The predictor X cannot contain NAs.")
  }

  if (!(is.matrix(X) & is.numeric(X))){
    stop("The predictor X must be a numeric matrix.")
  }

  return(TRUE)

}


validate_Y <- function(Y){
  ## numeric_vector -> boolean
  ## produce true if Y is a numeric vector with no missing values

  return(TRUE)
}

validate_fn_args <- function(fn, lst){ # !!! test
  ## function list -> boolean
  ## produce true if elements lst are valid arguments for fn

  arg1 <- deparse(substitute(fn))
  arg2 <- deparse(substitute(lst))

  required_args <- formals(fn)
  cond <- names(lst) %in% names(required_args)

  if (!all(cond)){
    stop(paste0(arg2, " contains the wrong arguments for ", arg1,
                ". These are: ", names(lst)[!cond]))
  } else {
    return(TRUE)
  }
}

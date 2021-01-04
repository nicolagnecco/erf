validate_fn <- function(fn){
  ## function -> boolean
  ## produce TRUE if fn is a function

  arg <- deparse(substitute(fn))

  if (!(is.function(fn))){
    stop(paste0(fn, " is not a function."))
  }

  return(TRUE)
}

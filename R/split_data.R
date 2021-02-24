split_data <- function(x, folds){
  ## numeric_vector|matrix numeric_vector -> list
  ## produce list with test and train sets
  lst <- list()

  if (length(dim(x)) == 0) {
    lst$test <- x[folds]
    lst$train <- x[-folds]
  } else {
    lst$test <- x[folds, , drop = FALSE]
    lst$train <- x[-folds, , drop = FALSE]
  }

  return(lst)
}

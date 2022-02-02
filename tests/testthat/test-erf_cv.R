test_that("erf_cv works", {
  library(foreach)
  n <- 1e2
  n_rep <- 10
  K <- 5
  min.node.size <- c(5, 40, 100)
  lambda <- c(0, 0.001, 0.01)

  myfolds <- get_repeated_k_folds(n, K, n_rep)

  fit_and_score <- function(X, Y, folds, ...){

    # grf::quantile_forest(X, Y, num.trees = 50)

    paste0(sum(folds), " ",  my_fun(...))

  }

  my_fun <- function(min.node.size, lambda) {
    glue::glue("min.node.size = {min.node.size}; lambda = {lambda}.")
  }

  splits <- myfolds

  params <- tidyr::expand_grid(
    min.node.size = min.node.size,
    lambda = lambda
  )

  a <- tidyr::crossing(splits, params)

  params <- tidyr::expand_grid(
    fold_id = seq_len(K),
    rep_id = seq_len(n_rep),
    min.node.size = min.node.size,
    lambda = lambda
  )

  full_grid <- splits %>%
    dplyr::left_join(params, by = c("rep_id", "fold_id"))

  fun_args <- full_grid %>%
    dplyr::select(-rep_id, -fold_id)

  all.equal(a, full_grid)

  fit_and_score_partial <- purrr::partial(fit_and_score, X = X, Y = Y)

  res1 <- function() {
    purrr::pmap(fun_args, fit_and_score_partial)
  }

  res2 <- function() {
    purrr::map(1:nrow(fun_args), function(i) {
      x <- fun_args[i, ]
      fit_and_score_partial(x$folds[[1]], x$min.node.size, x$lambda)
    })
  }

  res3 <- function() {
    purrr::map(1:nrow(fun_args), function(i) {

      folds <- fun_args$folds[[i]]
      min.node.size <- fun_args$min.node.size[i]
      lambda <- fun_args$lambda[i]

      fit_and_score_partial(folds, min.node.size, lambda)

    })
  }

  res4 <- function() {
    foreach(
      i = 1:nrow(fun_args),
      .options.future = list(scheduling = FALSE)
    ) %dopar% {

      fit_and_score_partial <- purrr::partial(fit_and_score, X = X, Y = Y)

      folds <- fun_args$folds[[i]]
      min.node.size <- fun_args$min.node.size[i]
      lambda <- fun_args$lambda[i]

      fit_and_score_partial(folds, min.node.size, lambda)

    }
  }


  bb <- bench::mark(
    res1(),
    res2(),
    res3(),
    res4(),
    filter_gc = FALSE, iterations = 6
  )

})

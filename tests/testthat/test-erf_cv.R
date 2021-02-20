test_that("erf_cv works", {
  library(foreach)
  n <- 1e2
  n_rep <- 10
  K <- 5
  min.node.size <- c(5, 40, 100)
  lambda <- c(0, 0.001, 0.01)

  myfolds <- create_folds(n, n_rep, K)

  fit_and_score <- function(X, Y, folds, ...){

    # grf::quantile_forest(X, Y, num.trees = 50)

    paste0(sum(folds), " ",  my_fun(...))

  }

  my_fun <- function(min.node.size, lambda) {
    glue::glue("min.node.size = {min.node.size}; lambda = {lambda}.")
  }


  res1 <- function() {
    splits <- tibble::tibble(folds = myfolds) %>%
      dplyr::mutate(rep_id = seq_len(n_rep)) %>%
      tidyr::unnest(folds) %>%
      dplyr::mutate(fold_id = rep(seq_len(K), n_rep))

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
      dplyr::left_join(params, by = c("rep_id", "fold_id")) %>%
      dplyr::select(-rep_id, -fold_id)

    fit_and_score_partial <- purrr::partial(fit_and_score, X = X, Y = Y)

    purrr::pmap(full_grid, fit_and_score_partial)
  }

  res2 <- function() {
    splits <- tibble::tibble(folds = myfolds) %>%
      dplyr::mutate(rep_id = seq_len(n_rep)) %>%
      tidyr::unnest(folds) %>%
      dplyr::mutate(fold_id = rep(seq_len(K), n_rep))


    params <- tidyr::expand_grid(
      fold_id = seq_len(K),
      rep_id = seq_len(n_rep),
      min.node.size = min.node.size,
      lambda = lambda
    )

    full_grid <- splits %>%
      dplyr::left_join(params, by = c("rep_id", "fold_id")) %>%
      dplyr::select(-rep_id, -fold_id)

    fit_and_score_partial <- purrr::partial(fit_and_score, X = X, Y = Y)

    purrr::map(1:nrow(full_grid), function(i) {
      x <- full_grid[i, ]
      fit_and_score_partial(x$folds[[1]], x$min.node.size, x$lambda)
    })
  }

  res3 <- function() {
    grid <- tidyr::expand_grid(
      rep_id = 1:n_rep, fold_id = 1:K,
      min.node.size = min.node.size,
      lambda = lambda
    )

    fit_and_score_partial <- purrr::partial(fit_and_score, X = X, Y = Y)

    purrr::map(1:nrow(grid), function(i) {

      rep_id <- grid$rep_id[i]
      fold_id <- grid$fold_id[i]
      min.node.size <- grid$min.node.size[i]
      lambda <- grid$lambda[i]

      fit_and_score_partial(myfolds[[rep_id]][[fold_id]], min.node.size, lambda)

    })
  }


  res4 <- function() {

    grid <- tidyr::expand_grid(
      rep_id = 1:n_rep, fold_id = 1:K,
      min.node.size = min.node.size,
      lambda = lambda
    )

    foreach(
      i = 1:nrow(grid),
      .options.future = list(scheduling = FALSE)
    ) %dopar% {

      fit_and_score_partial <- purrr::partial(fit_and_score, X = X, Y = Y)

      rep_id <- grid$rep_id[i]
      fold_id <- grid$fold_id[i]
      min.node.size <- grid$min.node.size[i]
      lambda <- grid$lambda[i]

      fit_and_score_partial(myfolds[[rep_id]][[fold_id]], min.node.size, lambda)

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

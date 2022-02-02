get_repeated_k_folds <- function(n, K, nreps, seed = NULL) {
  ## numeric (4x) -> tibble
  ## produce a tibble with nreps splits for K-fold CV
  ## columns are `folds`, `rep_id`, `fold_id`

  if (!is.null(seed)){
    set.seed(seed)
  }

  lst <- lapply(X = rep(1, nreps), FUN = function(x) {
    chunk(sample(1:n), K)
  })

  tibble::tibble(folds = lst) %>%
    dplyr::mutate(rep_id = seq_len(nreps)) %>%
    tidyr::unnest(.data$folds) %>%
    dplyr::mutate(fold_id = rep(seq_len(K), nreps))
}

chunk <- function(x, K) {
  ## numeric_vector integer -> list
  ## split x into K chunks
  unname(split(x, factor(sort(rank(x) %% K))))
}

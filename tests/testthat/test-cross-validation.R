# n <- 2000
# p <- 3
# X <- matrix(runif(n * p, min = -1, max = 1), ncol = p)
# Y <- ifelse(X[, 1] > 0,
#             rt(n, df = 4) * 2,
#             rt(n, df = 4))
# fit.grf <- grf::quantile_forest(X, Y)
# t_xi <- compute_thresholds(object = fit.grf, threshold = 0.8, X = X,
#                            out_of_bag = TRUE)
#
# args_grf <- list(quantiles = c(0.1, 0.5, 0.9))
# min.node.size <- c(5, 40, 100, 1000)
#
#
# test_that("multiplication works", {
#   debug(erf_cv)
#   res <- erf_cv(X, Y, t_xi, threshold = 0.8, min.node.size = min.node.size,
#                 K = 5, n_rep = 1, args_grf = args_grf, verbose = TRUE)
#
#   res2plot <- res %>%
#     dplyr::group_by(min.node.size) %>%
#     dplyr::summarise(cv_err = mean(cv_K_fold_out),
#                      cv_se = 1/sqrt(5) * sd(cv_K_fold_out))
# })
#
# library(ggplot2)
#
# ggplot(res %>% dplyr::mutate(min.node.size = factor(min.node.size))) +
#   geom_point(aes(x = min.node.size, y = cv_K_fold_out))
#
# ggplot(res2plot %>% dplyr::mutate(min.node.size = factor(min.node.size))) +
#   geom_point(aes(x = min.node.size, y = cv_err))

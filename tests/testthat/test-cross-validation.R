# n <- 1000
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
#   res <- erf_cv(X, Y, t_xi, min.node.size, K = 5, n_rep = 5, args_grf = args_grf)
# })
#
# library(ggplot2)
#
# ggplot(res %>% dplyr::mutate(min.node.size = factor(min.node.size))) +
#   geom_point(aes(x = min.node.size, y = cv_err))

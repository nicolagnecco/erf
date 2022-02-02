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
# lambda <- c(0, .1)
#
#
# test_that("multiplication works", {
#   debugonce(erf_cv_deprecated)
#   res <- erf_cv_deprecated(X, Y, t_xi, threshold = 0.8, min.node.size = min.node.size,
#                 lambda = lambda,
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



# Ideal usage of the package
# 1. Fit erf
# erf.fit <- erf(X, Y, min.node.size = 40, lambda = 0.001, base_threshold = NULL, ...)
#
# predict(erf.fit, newdata = X_test, quantiles = c(0.999), ...) # ... any argument for predict(base_threshold, ...) e.g., base_levels = 0.8
#
# plot(erf.fit) # diagnostic plot
# print(erf.fit) # variable importance
#

# 2. CV erf
# erf.fit.cv <- erf_cv(X, Y,
#                   min.node.size = c(5, 40, 100),
#                   lambda = c(0, 0.001, 0.1, 1),
#                   nfolds = 5, nrep = 3,
#                   base_threshold = NULL,
#                   ...)
#
# predict(erf.fit.cv, newdata = X_test, quantiles = c(0.999), ...)
#
# plot(erf.fit.cv) # ???
# print(erf.fit.cv) # ???
#

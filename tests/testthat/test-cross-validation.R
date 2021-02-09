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
#   debugonce(erf_cv)
#   res <- erf_cv(X, Y, t_xi, threshold = 0.8, min.node.size = min.node.size,
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

# Understand classes
n <- 2000
p <- 3
X <- matrix(runif(n * p, min = -1, max = 1), ncol = p)
Y <- ifelse(X[, 1] > 0,
            rt(n, df = 4) * 2,
            rt(n, df = 4))
fit.grf <- grf::quantile_forest(X, Y, min.node.size = -10)

library(methods)
class(fit.grf)
methods(class = "quantile_forest")
grf:::print.grf

print(fit.grf)

grf::get_sample_weights(fit.grf)

rboot <- list(month = "August", year = 2015, instructor = "Nicola", attendance = 100)
class(rboot) <- "workshop"

rboot
is(rboot, "workshop")
coef.default
print.workshop <- function(x){
  with(x,
       cat("A class held in ", month, " ", year, ". The instructor was ",
           instructor, ". The course attendance was ", attendance, ".\n",
           sep = ""))
}

print(rboot)

a <- glm(Y ~ X, method = "model.frame")
class(a)

grf::tune_causal_forest()

str(fit.grf)

cv.glmnet()

?glmnet::glmnet
?glmnet::cv.glmnet()
?grf::predict.quantile_forest
predict_erf

###
lm
?args(lm)
?formals(lm)
myf <- function(x = 2){
  print(missing(x))
  invisible(x)
}

myf()
?missing



n <- 2000
p <- 3
X <- matrix(runif(n * p, min = -1, max = 1), ncol = p)
Y <- ifelse(X[, 1] > 0,
            rt(n, df = 4) * 2,
            rt(n, df = 4))
grf.fit <- grf::quantile_forest(X, Y)

# R6
erf <- extreme_forest(min.node.size = 40, lambda = 0.001,
                      base_threshold = grf.fit)

erf$fit(X, Y)

erf$predict(newdata = X_test, quantile_levels = c(0.999), base_levels = 0.8)




# example
library("R6")
Queue <- R6Class("Queue",
                 public = list(
                   initialize = function(...) {
                     for (item in list(...)) {
                       self$add(item)
                     }
                   },
                   add = function(x) {
                     private$queue <- c(private$queue, list(x))
                     invisible(self)
                   },
                   remove = function() {
                     if (private$length() == 0) return(NULL)
                     # Can use private$queue for explicit access
                     head <- private$queue[[1]]
                     private$queue <- private$queue[-1]
                     head
                   }
                 ),
                 private = list(
                   queue = list(),
                   length = function() base::length(private$queue)
                 )
)


a <- Queue$new(2, 3, 2)

q <- Queue$new(5, 6, "foo")

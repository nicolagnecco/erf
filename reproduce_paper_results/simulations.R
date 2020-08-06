rm(list = ls())
library(tidyverse)
library(grf)
library(erf)
library(doParallel)
library(doRNG)
library(tictoc)
source("reproduce_paper_results/simulation_functions.R")


# Simulation arguments
log_file <- "reproduce_paper_results/output/sims.txt"
n <- 2000
p <- 40
ntest <- 100
threshold <- 0.8

settings <- set_simulations(seed = 42)
sims_args <- settings$simulation_arguments
rm(settings)

m <- NROW(sims_args)

# Loop through all simulations
tic()
sink(file = log_file)
cat("**** Simulation **** \n")
ll <- map_dfr(1:m, wrapper_sim, sims_args)
sink()
closeAllConnections()
toc()

# save results
saveRDS(ll, file = "reproduce_paper_results/output/simulations.rds")


# simulation function # !!! ####
# output_tbl <- tibble(training_id = integer(0),
#                      x = double(0),
#                      method = character(0),
#                      predictions = double(0),
#                      model = character(0),
#                      alpha = double(0),
#                      df = double(0))
#
#
# # run simulations
# for (i in seq_len(nrow(my_args))){
#
#   cat("Simulation", i, "out of", nrow(my_args),"\n")
#
#   # set current simulation variables
#   training_id <- my_args$training_id[i]
#   alpha <- my_args$alphas[i]
#   model <- my_args$model[i]
#   df <- my_args$df[i]
#
#   # generate training data
#   dat <- generate_joint_distribution(n = n, p = p, model = model, df = df)
#
#   # generate test data
#   x_test <- matrix(0, nrow = ntest, ncol = p)
#   x_test[, 1] <- seq(-1, 1, length.out = ntest)
#
#   # produce true quantile function
#   q_true <- generate_theoretical_quantiles(alpha = alpha, x = x_test[, 1],
#                                            model = model, df = df)
#
#   # fit quantile regression function
#   fit_grf <- quantile_forest(dat$X, dat$Y, quantiles = c(0.1, 0.5, 0.9))
#
#   # predict quantile regression function
#   predictions_grf <- predict(fit_grf, x_test, quantiles = alpha)
#   predictions_erf <- predict_erf(fit_grf, quantiles = alpha,
#                                  threshold = 0.8,
#                                  newdata = x_test, model_assessment = FALSE,
#                                  Y.test = NULL,
#                                  out_of_bag = FALSE)$predictions
#
#   # collect results
#   tb1 <- tibble(training_id = training_id,
#          model = model,
#          alpha = alpha,
#          df = df)
#
#   tb_erf <- tibble(training_id = training_id,
#                     x = x_test[, 1],
#                     method = "erf",
#                     predictions = predictions_erf[, 1])
#
#   tb_grf <- tibble(training_id = training_id,
#                    x = x_test[, 1],
#                    method = "grf",
#                    predictions = predictions_grf[, 1])
#
#   tb_true <- tibble(training_id = training_id,
#                     x = x_test[, 1],
#                     method = "true",
#                     predictions = q_true[, 1])
#
#
#   output_tbl <- bind_rows(tb_true, tb_grf, tb_erf) %>%
#     left_join(tb1, by = "training_id") %>%
#     bind_rows(output_tbl)
#
# }
#
# saveRDS(output_tbl, "reproduce_paper_results/output/simulations.rds")
#
# # plot results
# dat <- output_tbl %>%
#   mutate(method = factor(method),
#          training_id = factor(training_id),
#          model = if_else(model == "gaussian", model,
#                          paste(model, "_", df, sep = "")))
#
# models <- unique(dat$model)
#
# for (i in seq_along(models)){
#   m <- models[i]
#
#   dat_plot <- dat %>%
#     filter(model == m)
#
#   dat_methods <- dat_plot %>%
#     filter(method != "true")
#
#   dat_quantile_function <- dat_plot %>%
#     filter(method == "true")
#
#
#   gg <- ggplot() +
#     geom_line(data = dat_methods,
#               aes(x = x, y = predictions, col = method,
#                   group = interaction(training_id, method)),
#               alpha = .3) +
#     geom_step(data = dat_quantile_function,
#               aes(x = x, y = predictions)) +
#     scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7")) +
#     facet_grid(rows = vars(alpha), scales = "free")
#
#   ggsave(paste("reproduce_paper_results/output/simulation_", m, ".pdf", sep = ""),
#          gg, width = 10, height = 7.5, units = c("in"))
#
# }
#



# Draft ####
# for each training sample, and quantile, distribution
#
# data_generating_process
# X_train, Y_train -> simulate data
# Q -> produce true quantile function
#
# fit regression function
# fit_grf
#
# predict based on regression function
# X_test
# predict_grf -> Q_hat1
# predict_erf -> Q_hat2
#
# plot
# evaluate 1
# Y_test
# compute_E_Y_X_x{l(Y, Q)}
#
# evaluate 2
# Q_hat1 vs Q
#


rm(list = ls())
library(tidyverse)
source("reproduce_paper_results/simulation_functions.R")

# plot results ####
dat <- read_rds("reproduce_paper_results/output/simulations_092320.rds")

n0 <- 2e3
p0 <- 40
scale0 <- 2
num.trees0 <- 2e3
min.node.size0 <- 5
honesty0 <- TRUE
threshold0 <- 0.8
out_of_bag0 <- FALSE
test_data0 <- "zero"

## min.node.size ####
dat_plot <- dat %>%
  filter(num.trees == num.trees0 , test_data == test_data0) %>%
  mutate(method = factor(method),
         model = if_else(model == "gaussian", model,
                         paste(model, "_", df, sep = ""))) %>%
  select(nexp, method, model, quantiles_predict, predictions, min.node.size, x) %>%
  unnest(cols = c(quantiles_predict, predictions)) %>%
  pivot_wider(names_from = "method",
              values_from = "predictions") %>%
  mutate(grf = (grf - true)^2,
         erf = (erf - true)^2) %>%
  select(-true) %>%
  pivot_longer(cols = c("grf", "erf"),
               names_to = "method", values_to = "se") %>%
  group_by(model, quantiles_predict, method, min.node.size, nexp) %>%
  summarise(ise = mean(se)) %>% # mean across x's
  # filter(method == "erf", quantiles_predict == .9995, min.node.size==40)
  # filter(method == "grf", quantiles_predict == .999)
  summarise(mise = mean(ise),
            upper_bound = quantile(ise, .9),
            lower_bound = quantile(ise, .1)) %>%
            # lower_bound = sqrt(max(mise - 1 * sd(ise), 0))) %>%
  mutate(method = factor(method))


gg <- ggplot(dat_plot %>% filter(quantiles_predict == .9995),
             aes(x = min.node.size, y = mise, col = method)) +
  facet_grid(model ~ quantiles_predict, scale = "free") +
  geom_line(size = 1, alpha = .5) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin=lower_bound, ymax=upper_bound, x=min.node.size, fill = method), alpha = 0.3)+
  scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7")) +
  scale_fill_manual(values = c("#D55E00","#0072B2", "#CC79A7")); gg

ggsave(paste("reproduce_paper_results/output/simulation_", "min_node_size", ".pdf", sep = ""),
       gg, width = 10, height = 7.5, units = c("in"))

# !!! look at the unconditional very high quantile (higher min.node.size)

## num.trees ####
dat_plot <- dat %>%
  filter(min.node.size == min.node.size0 , test_data == test_data0) %>%
  mutate(method = factor(method),
         model = if_else(model == "gaussian", model,
                         paste(model, "_", df, sep = ""))) %>%
  select(nexp, method, model, quantiles_predict, predictions, num.trees, x) %>%
  unnest(cols = c(quantiles_predict, predictions)) %>%
  pivot_wider(names_from = "method",
              values_from = "predictions") %>%
  mutate(grf = (grf - true)^2,
         erf = (erf - true)^2) %>%
  select(-true) %>%
  pivot_longer(cols = c("grf", "erf"),
               names_to = "method", values_to = "se") %>%
  group_by(model, quantiles_predict, method, num.trees, nexp) %>%
  summarise(ise = mean(se)) %>% # mean across x's
  # filter(method == "grf", quantiles_predict == .999)
  summarise(mise = mean(ise),
            upper_bound = quantile(ise, .9),
            lower_bound = quantile(ise, .1)) %>%
  mutate(method = factor(method))


gg <- ggplot(dat_plot %>% filter(quantiles_predict == .9995),
             aes(x = num.trees, y = mise, col = method)) +
  facet_grid(model ~ quantiles_predict, scale = "free") +
  geom_line(size = 1, alpha = .5) +
  geom_point(size = 3) +
  geom_ribbon(aes(ymin=lower_bound, ymax=upper_bound, x=num.trees,
                  fill = method), alpha = 0.2)+
  scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7")) +
  scale_fill_manual(values = c("#D55E00","#0072B2", "#CC79A7")); gg

ggsave(paste("reproduce_paper_results/output/simulation_", "num_trees", ".pdf", sep = ""),
       gg, width = 10, height = 7.5, units = c("in"))


dat_plot <- dat %>%
  filter(min.node.size == 5 , num.trees == 2e3)

table(dat_plot$test_data)


# plot results ####
dat <- read_rds("reproduce_paper_results/output/simulations.rds")

n0 <- 2e3
p0 <- 40
scale0 <- 2
num.trees0 <- 2e3
min.node.size0 <- 5
honesty0 <- TRUE
threshold0 <- 0.8
out_of_bag0 <- FALSE

## sample size ####
dat_plot <- dat %>%
  filter(p %in% p0 , scale %in% scale0 ,
         num.trees %in% num.trees0 , min.node.size %in% min.node.size0 ,
         honesty %in% honesty0 , threshold %in% threshold0 ,
         out_of_bag %in% out_of_bag0) %>%
  mutate(method = factor(method),
         # training_id = factor(training_id),
         model = if_else(model == "gaussian", model,
                         paste(model, "_", df, sep = ""))) %>%
  select(nexp, method, model, quantiles_predict, predictions, n, x) %>%
  unnest(cols = c(quantiles_predict, predictions)) %>%
  pivot_wider(names_from = "method",
              values_from = "predictions") %>%
  mutate(grf = (grf - true)^2,
         erf = (erf - true)^2) %>%
  select(-true) %>%
  pivot_longer(cols = c("grf", "erf"),
               names_to = "method", values_to = "se") %>%
  group_by(model, quantiles_predict, n, method, x) %>%
  summarise(mse = mean(se)) %>%     # mean across nexp
  summarise(rimse = sqrt(mean(mse)))  %>%  # mean across x's
  mutate(method = factor(method))


gg <- ggplot(dat_plot %>% filter(quantiles_predict >= .99), aes(x = n, y = rimse, col = method)) +
  facet_grid(model ~ quantiles_predict, scale = "free") +
  geom_line(size = 1, alpha = .5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7"))

ggsave(paste("reproduce_paper_results/output/simulation_", "n", ".pdf", sep = ""),
       gg, width = 10, height = 7.5, units = c("in"))



## dimension ####
dat_plot <- dat %>%
  filter(n %in% n0 , scale %in% scale0 ,
         num.trees %in% num.trees0 , min.node.size %in% min.node.size0 ,
         honesty %in% honesty0 , threshold %in% threshold0 ,
         out_of_bag %in% out_of_bag0) %>%
  mutate(method = factor(method),
         # training_id = factor(training_id),
         model = if_else(model == "gaussian", model,
                         paste(model, "_", df, sep = ""))) %>%
  select(nexp, method, model, quantiles_predict, predictions, p, x) %>%
  unnest(cols = c(quantiles_predict, predictions)) %>%
  pivot_wider(names_from = "method",
              values_from = "predictions") %>%
  mutate(grf = (grf - true)^2,
         erf = (erf - true)^2) %>%
  select(-true) %>%
  pivot_longer(cols = c("grf", "erf"),
               names_to = "method", values_to = "se") %>%
  group_by(model, quantiles_predict, p, method, x) %>%
  summarise(mse = mean(se)) %>%     # mean across nexp
  summarise(rimse = sqrt(mean(mse)))  %>%  # mean across x's
  mutate(method = factor(method))


gg <- ggplot(dat_plot %>% filter(quantiles_predict >= .99), aes(x = p, y = rimse, col = method)) +
  facet_grid(model ~ quantiles_predict, scale = "free") +
  geom_line(size = 1, alpha = .5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7"))


ggsave(paste("reproduce_paper_results/output/simulation_", "p", ".pdf", sep = ""),
       gg, width = 10, height = 7.5, units = c("in"))

## scale ####
dat_plot <- dat %>%
  filter(n %in% n0 , p %in% p0,
         num.trees %in% num.trees0 , min.node.size %in% min.node.size0 ,
         honesty %in% honesty0 , threshold %in% threshold0 ,
         out_of_bag %in% out_of_bag0) %>%
  mutate(method = factor(method),
         # training_id = factor(training_id),
         model = if_else(model == "gaussian", model,
                         paste(model, "_", df, sep = ""))) %>%
  select(nexp, method, model, quantiles_predict, predictions, scale, x) %>%
  unnest(cols = c(quantiles_predict, predictions)) %>%
  pivot_wider(names_from = "method",
              values_from = "predictions") %>%
  mutate(grf = (grf - true)^2,
         erf = (erf - true)^2) %>%
  select(-true) %>%
  pivot_longer(cols = c("grf", "erf"),
               names_to = "method", values_to = "se") %>%
  group_by(model, quantiles_predict, scale, method, x) %>%
  summarise(mse = mean(se)) %>%     # mean across nexp
  summarise(rimse = sqrt(mean(mse)))  %>%  # mean across x's
  mutate(method = factor(method))


gg <- ggplot(dat_plot %>% filter(quantiles_predict >= .99),
       aes(x = scale, y = rimse, col = method)) +
  facet_grid(model ~ quantiles_predict, scale = "free") +
  geom_line(size = 1, alpha = .5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7"))


ggsave(paste("reproduce_paper_results/output/simulation_", "scale", ".pdf", sep = ""),
       gg, width = 10, height = 7.5, units = c("in"))


## num.trees ####
dat_plot <- dat %>%
  filter(n %in% n0 , p %in% p0, scale %in% scale0 ,
         min.node.size %in% min.node.size0 ,
         honesty %in% honesty0 , threshold %in% threshold0 ,
         out_of_bag %in% out_of_bag0) %>%
  mutate(method = factor(method),
         # training_id = factor(training_id),
         model = if_else(model == "gaussian", model,
                         paste(model, "_", df, sep = ""))) %>%
  select(nexp, method, model, quantiles_predict, predictions, num.trees, x) %>%
  unnest(cols = c(quantiles_predict, predictions)) %>%
  pivot_wider(names_from = "method",
              values_from = "predictions") %>%
  mutate(grf = (grf - true)^2,
         erf = (erf - true)^2) %>%
  select(-true) %>%
  pivot_longer(cols = c("grf", "erf"),
               names_to = "method", values_to = "se") %>%
  group_by(model, quantiles_predict, num.trees, method, x) %>%
  summarise(mse = mean(se)) %>%     # mean across nexp
  summarise(rimse = sqrt(mean(mse)))  %>%  # mean across x's
  mutate(method = factor(method))


gg <- ggplot(dat_plot %>% filter(quantiles_predict >= .99), aes(x = num.trees, y = rimse, col = method)) +
  facet_grid(model ~ quantiles_predict, scale = "free") +
  geom_line(size = 1, alpha = .5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7"))


ggsave(paste("reproduce_paper_results/output/simulation_", "numtrees", ".pdf", sep = ""),
       gg, width = 10, height = 7.5, units = c("in"))

## min.node.size ####
dat_plot <- dat %>%
  filter(n %in% n0 , p %in% p0, scale %in% scale0 ,
         num.trees %in% num.trees0,
         honesty %in% honesty0 , threshold %in% threshold0 ,
         out_of_bag %in% out_of_bag0) %>%
  mutate(method = factor(method),
         # training_id = factor(training_id),
         model = if_else(model == "gaussian", model,
                         paste(model, "_", df, sep = ""))) %>%
  select(nexp, method, model, quantiles_predict, predictions, min.node.size, x) %>%
  unnest(cols = c(quantiles_predict, predictions)) %>%
  pivot_wider(names_from = "method",
              values_from = "predictions") %>%
  mutate(grf = (grf - true)^2,
         erf = (erf - true)^2) %>%
  select(-true) %>%
  pivot_longer(cols = c("grf", "erf"),
               names_to = "method", values_to = "se") %>%
  group_by(model, quantiles_predict, min.node.size, method, x) %>%
  summarise(mse = mean(se)) %>%     # mean across nexp
  summarise(rimse = sqrt(mean(mse)))  %>%  # mean across x's
  mutate(method = factor(method))


gg <- ggplot(dat_plot %>% filter(quantiles_predict >= .99), aes(x = min.node.size, y = rimse, col = method)) +
  facet_grid(model ~ quantiles_predict, scale = "free") +
  geom_line(size = 1, alpha = .5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7"))

ggsave(paste("reproduce_paper_results/output/simulation_", "minnodesize", ".pdf", sep = ""),
       gg, width = 10, height = 7.5, units = c("in"))




## honesty ####
dat_plot <- dat %>%
  filter(n %in% n0 , p %in% p0, scale %in% scale0 ,
         num.trees %in% num.trees0, min.node.size %in% min.node.size0,
         threshold %in% threshold0 ,
         out_of_bag %in% out_of_bag0) %>%
  mutate(method = factor(method),
         # training_id = factor(training_id),
         model = if_else(model == "gaussian", model,
                         paste(model, "_", df, sep = ""))) %>%
  select(nexp, method, model, quantiles_predict, predictions, honesty, x) %>%
  unnest(cols = c(quantiles_predict, predictions)) %>%
  pivot_wider(names_from = "method",
              values_from = "predictions") %>%
  mutate(grf = (grf - true)^2,
         erf = (erf - true)^2) %>%
  select(-true) %>%
  pivot_longer(cols = c("grf", "erf"),
               names_to = "method", values_to = "se") %>%
  group_by(model, quantiles_predict, honesty, method, x) %>%
  summarise(mse = mean(se)) %>%     # mean across nexp
  summarise(rimse = sqrt(mean(mse)))  %>%  # mean across x's
  mutate(method = factor(method),
         honesty = as.numeric(honesty))


gg <- ggplot(dat_plot %>% filter(quantiles_predict >= .99), aes(x = honesty, y = rimse, col = method)) +
  facet_grid(model ~ quantiles_predict, scale = "free") +
  geom_line(size = 1, alpha = .5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7"))

ggsave(paste("reproduce_paper_results/output/simulation_", "honesty", ".pdf", sep = ""),
       gg, width = 10, height = 7.5, units = c("in"))

## threshold ####
dat_plot <- dat %>%
  filter(n %in% n0 , p %in% p0, scale %in% scale0 ,
         num.trees %in% num.trees0, min.node.size %in% min.node.size0,
         honesty %in% honesty0,
         out_of_bag %in% out_of_bag0) %>%
  mutate(method = factor(method),
         # training_id = factor(training_id),
         model = if_else(model == "gaussian", model,
                         paste(model, "_", df, sep = ""))) %>%
  select(nexp, method, model, quantiles_predict, predictions, threshold, x) %>%
  unnest(cols = c(quantiles_predict, predictions)) %>%
  pivot_wider(names_from = "method",
              values_from = "predictions") %>%
  mutate(grf = (grf - true)^2,
         erf = (erf - true)^2) %>%
  select(-true) %>%
  pivot_longer(cols = c("grf", "erf"),
               names_to = "method", values_to = "se") %>%
  group_by(model, quantiles_predict, threshold, method, x) %>%
  summarise(mse = mean(se)) %>%     # mean across nexp
  summarise(rimse = sqrt(mean(mse)))  %>%  # mean across x's
  mutate(method = factor(method))


gg <- ggplot(dat_plot %>% filter(quantiles_predict >= .99), aes(x = threshold, y = rimse, col = method)) +
  facet_grid(model ~ quantiles_predict, scale = "free") +
  geom_line(size = 1, alpha = .5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7"))




ggsave(paste("reproduce_paper_results/output/simulation_", "threshold", ".pdf", sep = ""),
       gg, width = 10, height = 7.5, units = c("in"))



## out_of_bag ####
dat_plot <- dat %>%
  filter(n %in% n0 , p %in% p0, scale %in% scale0 ,
         num.trees %in% num.trees0, min.node.size %in% min.node.size0,
         honesty %in% honesty0 , threshold %in% threshold0) %>%
  mutate(method = factor(method),
         # training_id = factor(training_id),
         model = if_else(model == "gaussian", model,
                         paste(model, "_", df, sep = ""))) %>%
  select(nexp, method, model, quantiles_predict, predictions, out_of_bag, x) %>%
  unnest(cols = c(quantiles_predict, predictions)) %>%
  pivot_wider(names_from = "method",
              values_from = "predictions") %>%
  mutate(grf = (grf - true)^2,
         erf = (erf - true)^2) %>%
  select(-true) %>%
  pivot_longer(cols = c("grf", "erf"),
               names_to = "method", values_to = "se") %>%
  group_by(model, quantiles_predict, out_of_bag, method, x) %>%
  summarise(mse = mean(se)) %>%     # mean across nexp
  summarise(rimse = sqrt(mean(mse)))  %>%  # mean across x's
  mutate(method = factor(method),
         out_of_bag = as.numeric(out_of_bag))


gg <- ggplot(dat_plot %>% filter(quantiles_predict >= .99), aes(x = out_of_bag, y = rimse, col = method)) +
  facet_grid(model ~ quantiles_predict, scale = "free") +
  geom_line(size = 1, alpha = .5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7"))

ggsave(paste("reproduce_paper_results/output/simulation_", "outofbag", ".pdf", sep = ""),
       gg, width = 10, height = 7.5, units = c("in"))



# #####
# models <- unique(dat$model)
# i <- 1
#
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
#   gg <- ggplot() +
#     geom_ribbon(data = dat_methods,
#                 aes(x = x,
#                     ymin = lb, ymax = ub,
#                     fill = method),
#                 alpha = .25) +
#     geom_line(data = dat_methods,
#               aes(x = x, y = mean_quantile, color = method),
#               alpha = .8, size = 1) +
#     geom_step(data = dat_quantile_function,
#               aes(x = x, y = mean_quantile)) +
#     scale_fill_manual(values = c("#D55E00","#0072B2", "#CC79A7")) +
#     scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7")) +
#     facet_grid(rows = vars(alpha), scale = "free")
#
#   ggsave(paste("reproduce_paper_results/output/simulation_", m, ".pdf", sep = ""),
#          gg, width = 10, height = 7.5, units = c("in"))
#
# }

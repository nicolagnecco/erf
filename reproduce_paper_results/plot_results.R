rm(list = ls())
library(tidyverse)
source("reproduce_paper_results/simulation_functions.R")

# plot results
dat <- read_rds("reproduce_paper_results/output/simulations2.rds")

n0 <- 2e3
p0 <- 40
scale0 <- 2
num.trees0 <- 2e3
min.node.size0 <- 5
honesty0 <- TRUE
threshold0 <- 0.8
out_of_bag0 <- FALSE

aa <- dat %>%
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


ggplot(aa, aes(x = n, y = rimse, col = method)) +
  facet_grid(model ~ quantiles_predict, scale = "free") +
  geom_line(size = 1, alpha = .5) +
  geom_point(size = 3) +
  scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7"))


models <- unique(dat$model)
i <- 1


for (i in seq_along(models)){
  m <- models[i]

  dat_plot <- dat %>%
    filter(model == m)

  dat_methods <- dat_plot %>%
    filter(method != "true")

  dat_quantile_function <- dat_plot %>%
    filter(method == "true")

  gg <- ggplot() +
    geom_ribbon(data = dat_methods,
                aes(x = x,
                    ymin = lb, ymax = ub,
                    fill = method),
                alpha = .25) +
    geom_line(data = dat_methods,
              aes(x = x, y = mean_quantile, color = method),
              alpha = .8, size = 1) +
    geom_step(data = dat_quantile_function,
              aes(x = x, y = mean_quantile)) +
    scale_fill_manual(values = c("#D55E00","#0072B2", "#CC79A7")) +
    scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7")) +
    facet_grid(rows = vars(alpha), scale = "free")

  ggsave(paste("reproduce_paper_results/output/simulation_", m, ".pdf", sep = ""),
         gg, width = 10, height = 7.5, units = c("in"))

}

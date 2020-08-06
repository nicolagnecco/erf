rm(list = ls())
library(tidyverse)
source("reproduce_paper_results/simulation_functions.R")

# plot results
dat <- read_rds("reproduce_paper_results/output/simulations.rds") %>%
  mutate(method = factor(method),
         # training_id = factor(training_id),
         model = if_else(model == "gaussian", model,
                         paste(model, "_", df, sep = ""))) %>%
  select(-df) %>%
  group_by(method, x, model, alpha) %>%
  summarise(mean_quantile = mean(predictions),
            lb = quantile(predictions, .05),
            ub = quantile(predictions, .95))

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

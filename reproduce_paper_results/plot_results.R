rm(list = ls())
library(tidyverse)
library(cowplot)
source("reproduce_paper_results/simulation_functions.R")

GRAPH_TYPE <- "line"
theme_set(theme_bw() +
            theme(plot.background = element_blank(),
                  legend.background = element_blank()))

# function defintions ####
extract_params <- function(tbl, param, methods){
  ## dat character character_v -> tibble
  ## prepare data to be plotted

  base_params <- list(
    n0 = 2e3,
    p0 = 40,
    scale0 = 2,
    num.trees0 = 2e3,
    min.node.size0 = 5,
    honesty0 = TRUE,
    threshold0 = 0.8,
    out_of_bag0 = FALSE,
    test_data0 = "zero")

  base_params <- lapply(base_params, as.character)

  param_enquo <- as.name(param)

  base_params_names <- sub(pattern = "0", replacement = "", names(base_params))
  cond <- paste(unlist(base_params[which(base_params_names != param)]),
                collapse = "_")
  cols_to_unite <- base_params_names[base_params_names != param]

  dd <- tbl %>%
    unite("base_params", all_of(cols_to_unite)) %>%
    filter(base_params == cond)

  dd2 <- dd %>%
    mutate(model = if_else(model == "gaussian", model,
                           paste(model, "_", df, sep = ""))) %>%
    mutate(!!param_enquo := factor(!!param_enquo)) %>%
    select(nexp, method, model, quantiles_predict, predictions,
           !!param_enquo, x) %>%
    unnest(cols = c(quantiles_predict, predictions)) %>%
    pivot_wider(names_from = "method",
                values_from = "predictions")

  dd3 <- dd2 %>%
    mutate(across(all_of(methods), function(x){(x - true)^2})) %>%
    select(-true) %>%
    pivot_longer(cols = all_of(methods),
                 names_to = "method", values_to = "se") %>%
    group_by(model, quantiles_predict, method, !! param_enquo, nexp) %>%
    summarise(ise = sqrt(mean(se))) %>% # mean across x's
    mutate(method = factor(method))

  return(dd3)

}

plot_sims_0 <- function(tbl, quant){
  ## tibble -> plot
  ## create a plot for simulation_settings_0 given a quantile

  g <- ggplot(tbl %>% filter(quantile == quant),
               aes_string(y = "method", x = "ise", col = "method")) +
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", shape=17, size = 2) +
    scale_color_manual(values = c("#E69F00", "#D55E00","#0072B2", "#009E73")) +
    xlab("ISE") +
    ylab("") +
    ggtitle(paste("q =", quant))

  return(g)

}

plot_sims_1 <- function(tbl, param){
  ## tibble character -> list
  ## create list of simulation plots and and saves them

  quant_pred <- tbl$quantiles_predict %>% unique()
  lop <- list()

  if (GRAPH_TYPE == "boxplot"){

  for (i in seq_along(quant_pred)){
    g <- ggplot(tbl %>% filter(quantiles_predict == quant_pred[i])) +
      facet_grid(model ~ quantiles_predict, scale = "free") +
      geom_boxplot(aes_string(x = param, y = "ise", col = "method")) +
      scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7")) +
      ylab("ISE")

    ggsave(paste("reproduce_paper_results/output/simulation_", param,
                 "_quantile_", quant_pred[i], ".pdf", sep = ""),
           g, width = 10, height = 7.5, units = c("in"))

    lop[[i]] <- g

  }

  } else {

    tbl <- tbl %>% summarise(mise = mean(ise))

    for (i in seq_along(quant_pred)){
      g <- ggplot(tbl %>% filter(quantiles_predict == quant_pred[i]),
             aes_string(x = param,
                 y = "mise",
                 col = "method",
                 group = "method")) +
        facet_grid(model ~ quantiles_predict, scale = "free") +
        stat_summary(fun=sum, geom="line", size = 1, alpha = .5) +
        scale_color_manual(values = c("#D55E00","#0072B2", "#CC79A7")) +
        ylab("MISE")


      ggsave(paste("reproduce_paper_results/output/simulation_", param,
                   "_quantile_", quant_pred[i], ".pdf", sep = ""),
             g, width = 10, height = 7.5, units = c("in"))

      lop[[i]] <- g

    }

  }

  return(lop)
}


# plot results sim0 ####
dat <- read_rds("output/simulation_settings_0-2020-10-02_11_58_51.rds") %>%
  select(-quantiles_fit, -quantiles_predict, -rng) %>%
  unnest(cols = c(perf)) %>%
  mutate(method = factor(method),
         quantile = factor(quantiles_predict))

g1 <- plot_sims_0(dat, .99)
g2 <- plot_sims_0(dat, .995)
g3 <- plot_sims_0(dat, .999) + xlim(c(0, 110))
g4 <- plot_sims_0(dat, .9995) + xlim(c(0, 300))
g <- plot_grid(g1, g2, g3, g4, nrow = 2)
ggsave("output/simulation_settings_0.pdf", g,
       width = 15, height = 7.5, units = c("in"))


# plot results sim1 ####
dat <- read_rds("output/simulation_settings_0-2020-10-02_11_58_51.rds")

# n
dat_plot <- extract_params(dat, "n", c("grf", "erf"))
lop <- plot_sims_1(dat_plot, "n")

# p
dat_plot <- extract_params(dat, "p", c("grf", "erf"))
lop <- plot_sims_1(dat_plot, "p")

# scale
dat_plot <- extract_params(dat, "scale", c("grf", "erf"))
lop <- plot_sims_1(dat_plot, "scale")

# test_data
dat_plot <- extract_params(dat, "test_data", c("grf", "erf"))
lop <- plot_sims_1(dat_plot, "test_data")

# num.trees
dat_plot <- extract_params(dat, "num.trees", c("grf", "erf"))
lop <- plot_sims_1(dat_plot, "num.trees")

# min.node.size
dat_plot <- extract_params(dat, "min.node.size", c("grf", "erf"))
lop <- plot_sims_1(dat_plot, "min.node.size")

# honesty
dat_plot <- extract_params(dat, "honesty", c("grf", "erf"))
lop <- plot_sims_1(dat_plot, "honesty")

# threshold
dat_plot <- extract_params(dat, "threshold", c("grf", "erf"))
lop <- plot_sims_1(dat_plot, "threshold")

# out_of_bag
dat_plot <- extract_params(dat, "out_of_bag", c("grf", "erf"))
lop <- plot_sims_1(dat_plot, "out_of_bag")

# old plots #####
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

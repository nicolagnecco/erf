rm(list = ls())
library(tidyverse)
library(cowplot)
library(grid)
library(gridExtra)
source("simulation_functions.R")

theme_set(theme_bw() +
            theme(plot.background = element_blank(),
                  legend.background = element_blank()))

# function defintions ####
extract_params <- function(tbl, param, base_params, methods=NULL){
  ## dat character list character_v -> tibble
  ## prepare data to be plotted

  if (is.null(methods)){
    methods <- unique(tbl$perf[[1]]$method)
  }
  base_params <- lapply(base_params, as.character)

  param_enquo <- as.name(param)
  lvs <- if(is.double(tbl[[param]])){
    sort(unique(tbl[[param]]))
  } else {
    unique(tbl[[param]])
  }

  base_params_names <- sub(pattern = "0", replacement = "", names(base_params))
  cond <- paste(unlist(base_params[which(base_params_names != param)]),
                collapse = "_")
  cols_to_unite <- base_params_names[base_params_names != param]

  dd <- tbl %>%
    unite("base_params", all_of(cols_to_unite)) %>%
    filter(base_params == cond) %>%
    select(-quantiles_fit, -quantiles_predict, -rng)  %>%
    unnest(cols = c(perf)) %>%
    mutate(method = factor(method),
           quantile = factor(quantiles_predict))


  dd2 <- dd %>%
    mutate(distr = if_else(distr == "gaussian", distr,
                           paste(distr, "_", df, sep = ""))) %>%
    mutate(!!param_enquo := factor(!!param_enquo, levels = lvs))


  return(dd2)

}

plot_sims_0 <- function(tbl, quant){
  ## tibble -> plot
  ## create a plot for simulation_settings_0 given a quantile

  g <- ggplot(tbl %>% filter(quantile == quant),
               aes_string(y = "method", x = "ise", col = "method")) +
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", shape=17, size = 2) +
    scale_color_manual(values = c("#E69F00", "#D55E00","#0072B2", "#009E73")) +
    xlab("") +
    ylab("") +
    ggtitle(paste("q =", quant))

  return(g)

}

plot_sims_1 <- function(tbl, param){
  ## tibble character -> gtable
  ## create list of simulation plots and and saves them

  param_enquo <- as.name(param)
  quant_pred <- tbl$quantiles_predict %>% unique()
  distros <- tbl$distr %>% unique()
  lop <- list()
  k <- 0

  for (j in seq_along(distros)) {
    for (i in seq_along(quant_pred)){
      k <- k + 1

      dat2plot <- tbl %>% filter(quantiles_predict == quant_pred[i],
                                 distr == distros[j])

      upperylim <- dat2plot %>%
        group_by(method, !!param_enquo) %>%
        summarise(iqr = quantile(ise, .75) - quantile(ise, .25),
                  upper = quantile(ise, .75) + 1.5 * iqr, max(ise)) %>%
        ungroup() %>%
        select(upper) %>% max()

      g <- ggplot(dat2plot) +
        geom_boxplot(aes_string(x = param, y = "ise", col = "method"),
                     outlier.shape = NA) +
        scale_color_manual(values = c("#E69F00", "#D55E00","#0072B2", "#009E73")) +
        ylab("") +
        xlab("") +
        coord_cartesian(ylim = c(0, upperylim)) +
        ggtitle(paste("q = ", quant_pred[i],
                      "; distr = ", distros[[j]], sep = ""))

      lop[[k]] <- g

    }
  }

  pp <- plot_grid(plotlist = lop, ncol = length(distros),
                  nrow = length(quant_pred))

  #create common x and y labels
  y.grob <- textGrob("ISE",
                     gp=gpar(fontface="bold", fontsize=15), rot=90)

  x.grob <- textGrob(param,
                     gp=gpar(fontface="bold", fontsize=15))

  #add to plot
  gg <- grid.arrange(arrangeGrob(pp, left = y.grob, bottom = x.grob))

  ggsave(paste("output/simulation_settings_1_", param, ".pdf", sep = ""), gg,
         width = 22.5, height = 15, units = c("in"))
  return(gg)
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
pp <- plot_grid(g1, g2, g3, g4, nrow = 2)

#create common x and y labels
y.grob <- textGrob("method",
                   gp=gpar(fontface="bold", fontsize=15), rot=90)

x.grob <- textGrob("ISE",
                   gp=gpar(fontface="bold", fontsize=15))

#add to plot
gg <- grid.arrange(arrangeGrob(pp, left = y.grob, bottom = x.grob))

ggsave("output/simulation_settings_0.pdf", gg,
       width = 15, height = 7.5, units = c("in"))


# plot results sim1 ####
dat <- read_rds("output/simulation_settings_1-2020-10-05_11_12_42.rds")
base_params <- list(
  n0 = 2e3,
  p0 = 40,
  num.trees0 = 2e3,
  min.node.size0 = 5,
  honesty0 = TRUE,
  threshold0 = 0.8,
  out_of_bag0 = FALSE)


# n
dat_plot <- extract_params(dat, "n", base_params)
gg <- plot_sims_1(dat_plot, "n")


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

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
extract_params <- function(tbl, param, base_params){
  ## dat character list -> tibble
  ## prepare data to be plotted

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

plot_sims <- function(tbl, param, file_name){
  ## tibble character character -> gtable
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

  pp <- plot_grid(plotlist = lop, nrow = length(distros),
                  ncol = length(quant_pred))

  #create common x and y labels
  y.grob <- textGrob("ISE",
                     gp=gpar(fontface="bold", fontsize=15), rot=90)

  x.grob <- textGrob(param,
                     gp=gpar(fontface="bold", fontsize=15))

  #add to plot
  gg <- grid.arrange(arrangeGrob(pp, left = y.grob, bottom = x.grob))

  ggsave(paste0("output/", file_name, "_", param, ".pdf"), gg,
         width = 22.5, height = 15, units = c("in"))
  return(gg)
}

plot_grf_weights <- function(dat){
  ## tibble -> list of plots
  ## produce list of plots for grf weights for different test points x0

  dat <- dat %>%
    mutate(min.node.size =
             factor(min.node.size,
                    levels = sort(unique(dat$min.node.size)),
                    labels = paste0("min.node.size = ",
                                    sort(unique(dat$min.node.size)))),
           honesty = factor(paste0("honesty = ", honesty)))

  lop <- list()
  k <- 0

  for (x0_cur in unique(dat$x0)){
    k <- k + 1
    g <-  ggplot(dat %>% filter(x0 == x0_cur)) +
      facet_grid(honesty ~ min.node.size) +
      geom_line(aes(x = X1, y = weights),
                col = "#E69F00", alpha = 1) +
      geom_point(data = tibble(x = x0_cur, y = 0),
                 mapping = aes(x = x, y = y), size = 3)

    ggsave(paste0("output/grf_weights_x0=", x0_cur, ".pdf"), g,
           width = 10, height = 7.5, units = c("in"))

    lop[[k]] <- g

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
g3 <- plot_sims_0(dat, .999) + coord_cartesian(xlim = c(0, 110))
g4 <- plot_sims_0(dat, .9995) + coord_cartesian(xlim = c(0, 300))
pp <- plot_grid(g1, g2, g3, g4, nrow = 2)

#create common x and y labels
y.grob <- textGrob("method",
                   gp=gpar(fontface="bold", fontsize=15), rot=90)

x.grob <- textGrob("ISE",
                   gp=gpar(fontface="bold", fontsize=15))

# add to plot
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


param_names <- c("n", "p", "num.trees", "min.node.size", "honesty", "threshold",
                 "out_of_bag")
sim_name <- "simulation_settings_1"

for (prm in param_names){
  dat_plot <- extract_params(dat, prm, base_params)
  plot_sims(dat_plot, prm, file_name = sim_name)
}



# plot results sim2 ####
dat <- read_rds("output/simulation_settings_2-2020-10-06_05_25_16.rds")
base_params <- list(
  n0 = 2e3,
  p0 = 10,
  num.trees0 = 2e3,
  min.node.size0 = 5,
  honesty0 = TRUE,
  threshold0 = 0.8,
  out_of_bag0 = FALSE)

param_names <- c("n", "p", "num.trees", "min.node.size", "honesty", "threshold",
                 "out_of_bag")
sim_name <- "simulation_settings_2"


for (prm in param_names){
  dat_plot <- extract_params(dat, prm, base_params)
  plot_sims(dat_plot, prm, file_name = sim_name)
}



# plot grf weights ####
dat <- read_rds("output/simulation_grf_weights_sequential-2020-10-08_14_45_12.rds")
lop <- plot_grf_weights(dat)



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

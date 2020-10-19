library(tidyverse)
library(cowplot)
library(grid)
library(gridExtra)
source("simulation_functions.R")

theme_set(theme_bw() +
            theme(plot.background = element_blank(),
                  legend.background = element_blank(),
                  strip.background = element_rect(fill = "white")))

my_palette <- c("#D55E00", "#0072B2", "#009E73", "#E69F00", "#56B4E9",
                "#CC79A7")

# function definitions ####
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
    stat_summary(fun=mean, geom="crossbar", fatten = 1.5, width = .75,
                 color = "black") +
    scale_color_manual(values = my_palette) +
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
        scale_color_manual(values = my_palette) +
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
                col = my_palette[1], alpha = 1) +
      geom_point(data = tibble(x = x0_cur, y = 0),
                 mapping = aes(x = x, y = y), size = 3)

    ggsave(paste0("output/grf_weights_x0=", x0_cur, ".pdf"), g,
           width = 10, height = 7.5, units = c("in"))

    lop[[k]] <- g

  }

  return(lop)

}

plot_true_gpd_weights <- function(dat, distr = c("gaussian", "student_t")){
  ## tibble -> plot
  ## creates a plot for a given distribution

  arg_distr <- match.arg(distr)

  dat <- dat %>%
    mutate(min.node.size =
             factor(min.node.size,
                    levels = sort(unique(dat$min.node.size)),
                    labels = paste0("min.node.size = ",
                                    sort(unique(dat$min.node.size)))),
           quantiles_predict =
             factor(quantiles_predict,
                    levels = sort(unique(dat$quantiles_predict)),
                    labels = paste0("quantile = ",
                                    sort(unique(dat$quantiles_predict)))))

  ggplot(dat %>% filter(distr == arg_distr),
         aes(x = ise, y = method, col = method)) +
    facet_grid(min.node.size ~ quantiles_predict, scales = "free") +
    geom_boxplot() +
    stat_summary(fun=mean, geom="crossbar", fatten = 1.5, width = .75,
                 color = "black") +
    scale_color_manual(values = my_palette[c(1, 5, 6)]) +
    ggtitle(paste0("distribution = ", arg_distr))

}

plot_model <- function(dat, model, distr){
  ## tibble -> plot
  ## plot quantiles of model with given distr
  quantiles <- generate_theoretical_quantiles(alpha = .995, X = as.matrix(dat),
                                              model = model,
                                              distr = distr, df = NA) %>%
    as.numeric()


  dat <- dat %>%
    mutate(quantile = quantiles)

  g <- ggplot(dat, aes(x = X1, y = X2, fill = quantile)) +
    geom_raster() +
    coord_fixed(expand = FALSE) +
    scale_fill_viridis_c() +
    ggtitle(paste0("Model = ", model, "; distribution = ", distr))

  return(g)

}


# plot results sim0 ####
dat <- read_rds("output/simulation_settings_0-2020-10-12_12_55_29.rds") %>%
  select(-quantiles_fit, -quantiles_predict, -rng) %>%
  unnest(cols = c(perf)) %>%
  mutate(method = factor(method),
         quantile = factor(quantiles_predict))

# Gaussian distribution
dat_plot <- dat %>% filter(distr == "gaussian")
g1 <- plot_sims_0(dat_plot, .99)
g2 <- plot_sims_0(dat_plot, .995)
g3 <- plot_sims_0(dat_plot, .999) + coord_cartesian(xlim = c(0, 10))
g4 <- plot_sims_0(dat_plot, .9995) + coord_cartesian(xlim = c(0, 15))
g5 <- plot_sims_0(dat_plot, .9999) + coord_cartesian(xlim = c(0, 20))
pp <- plot_grid(g1, g2, g3, g4, g5, nrow = 3)

#create common x and y labels
y.grob <- textGrob("method",
                   gp=gpar(fontface="bold", fontsize=15), rot=90)

x.grob <- textGrob("ISE",
                   gp=gpar(fontface="bold", fontsize=15))

# add to plot
gg <- grid.arrange(arrangeGrob(pp, left = y.grob, bottom = x.grob))

ggsave("output/simulation_settings_0_gaussian.pdf", gg,
       width = 15, height = 12.5, units = c("in"))


# Student-t distribution
dat_plot <- dat %>% filter(distr == "student_t")
g1 <- plot_sims_0(dat_plot, .99)
g2 <- plot_sims_0(dat_plot, .995)
g3 <- plot_sims_0(dat_plot, .999) + coord_cartesian(xlim = c(0, 60))
g4 <- plot_sims_0(dat_plot, .9995) + coord_cartesian(xlim = c(0, 75))
g5 <- plot_sims_0(dat_plot, .9999) + coord_cartesian(xlim = c(0, 300))
pp <- plot_grid(g1, g2, g3, g4, g5, nrow = 3)

#create common x and y labels
y.grob <- textGrob("method",
                   gp=gpar(fontface="bold", fontsize=15), rot=90)

x.grob <- textGrob("ISE",
                   gp=gpar(fontface="bold", fontsize=15))

# add to plot
gg <- grid.arrange(arrangeGrob(pp, left = y.grob, bottom = x.grob))

ggsave("output/simulation_settings_0_student_t.pdf", gg,
       width = 15, height = 12.5, units = c("in"))


# plot results sim00 ####
dat <- read_rds("output/simulation_settings_00-2020-10-12_13_24_49.rds") %>%
  select(-quantiles_fit, -quantiles_predict, -rng) %>%
  unnest(cols = c(perf)) %>%
  mutate(method = factor(method),
         quantile = factor(quantiles_predict))

# Gaussian distribution
dat_plot <- dat %>% filter(distr == "gaussian")
g1 <- plot_sims_0(dat_plot, .99)
g2 <- plot_sims_0(dat_plot, .995)
g3 <- plot_sims_0(dat_plot, .999)
g4 <- plot_sims_0(dat_plot, .9995)
g5 <- plot_sims_0(dat_plot, .9999)

pp <- plot_grid(g1, g2, g3, g4, g5, nrow = 3)

#create common x and y labels
y.grob <- textGrob("method",
                   gp=gpar(fontface="bold", fontsize=15), rot=90)

x.grob <- textGrob("ISE",
                   gp=gpar(fontface="bold", fontsize=15))

# add to plot
gg <- grid.arrange(arrangeGrob(pp, left = y.grob, bottom = x.grob))

ggsave("output/simulation_settings_00_gaussian.pdf", gg,
       width = 15, height = 12.5, units = c("in"))


# Student-t distribution
dat_plot <- dat %>% filter(distr == "student_t")
g1 <- plot_sims_0(dat_plot, .99)
g2 <- plot_sims_0(dat_plot, .995)
g3 <- plot_sims_0(dat_plot, .999) + coord_cartesian(xlim = c(0, 60))
g4 <- plot_sims_0(dat_plot, .9995) + coord_cartesian(xlim = c(0, 160))
g5 <- plot_sims_0(dat_plot, .9999) + coord_cartesian(xlim = c(0, 450))

pp <- plot_grid(g1, g2, g3, g4, g5, nrow = 3)

#create common x and y labels
y.grob <- textGrob("method",
                   gp=gpar(fontface="bold", fontsize=15), rot=90)

x.grob <- textGrob("ISE",
                   gp=gpar(fontface="bold", fontsize=15))

# add to plot
gg <- grid.arrange(arrangeGrob(pp, left = y.grob, bottom = x.grob))

ggsave("output/simulation_settings_00_student_t.pdf", gg,
       width = 15, height = 12.5, units = c("in"))


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


# plot results sim3 ####
dat <- read_rds("output/simulation_settings_3-2020-10-09_09_00_48.rds")
dat_plot <- dat %>%
  ungroup() %>%
  select(perf, test_data) %>%
  unnest(cols = c(perf)) %>%
  mutate(method = factor(method),
         test_data = factor(test_data),
         quantiles_predict =
           factor(quantiles_predict,
                  levels = sort(unique(quantiles_predict)),
                  labels = paste0("quantile = ",
                                  sort(unique(quantiles_predict)))))


gg <- ggplot(dat_plot, aes(x = ise, y = method, col = method)) +
  facet_grid(test_data ~ quantiles_predict, scales = "free") +
  geom_boxplot() +
  stat_summary(fun=mean, geom="crossbar", fatten = 1.5, width = .75,
               color = "black") +
  scale_color_manual(values = my_palette)

ggsave("output/simulation_settings_3.pdf", gg,
         width = 10, height = 10, units = c("in"))


# plot grf weights ####
dat <- read_rds("output/simulation_grf_weights_sequential-2020-10-08_14_45_12.rds")
lop <- plot_grf_weights(dat)


# plot results sim4 ####
dat <- read_rds("output/simulation_settings_4-2020-10-12_12_30_43.rds")
dat_plot <- dat %>%
  ungroup() %>%
  select(perf, min.node.size, distr) %>%
  unnest(cols = c(perf)) %>%
  mutate(method = factor(method),
         distr = factor(distr))

g1 <- plot_true_gpd_weights(dat_plot %>% filter(min.node.size != 2000),
                            "gaussian"); g1
g2 <- plot_true_gpd_weights(dat_plot %>% filter(min.node.size != 2000),
                            "student_t"); g2
gg <- plot_grid(g1, g2, nrow = 2)

ggsave("output/simulation_settings_4.pdf", gg,
       width = 10, height = 15, units = c("in"))


# plot models ####
n <- 1e4
dat <- expand_grid(X1 = seq(-1, 1, length.out = floor(sqrt(n))),
                   X2 = seq(-1, 1, length.out = floor(sqrt(n))))

g1 <- plot_model(dat, "periodic", "gaussian")
g2 <- plot_model(dat, "periodic", "student_t")
g3 <- plot_model(dat, "gaussian", "gaussian")
g4 <- plot_model(dat, "gaussian", "student_t")

gg <- plot_grid(g1, g2, g3, g4, nrow = 2)
ggsave("output/models.pdf", gg,
       width = 10, height = 10, units = c("in"))



# plot results sim5 ####
dat <- read_rds("output/simulation_settings_5-2020-10-11_10_31_40.rds")
dat_plot <- dat %>%
  ungroup() %>%
  select(perf, ntest, model) %>%
  unnest(cols = c(perf)) %>%
  mutate(method = factor(method),
         ntest = factor(paste0("ntest = ", ntest)))

g1 <- ggplot(dat_plot %>% filter(model == "step"),
       aes(x = ise, y = method, col = method)) +
  facet_grid(ntest ~ quantiles_predict, scales = "free") +
  geom_boxplot() +
  stat_summary(fun=mean, geom="crossbar", fatten = 1.5, width = .75,
               color = "black") +
  scale_color_manual(values = my_palette) +
  ggtitle(paste0("Model = step"))
g2 <- ggplot(dat_plot %>% filter(model == "gaussian"),
             aes(x = ise, y = method, col = method)) +
  facet_grid(ntest ~ quantiles_predict, scales = "free") +
  geom_boxplot() +
  stat_summary(fun=mean, geom="crossbar", fatten = 1.5, width = .75,
               color = "black") +
  scale_color_manual(values = my_palette) +
  ggtitle(paste0("Model = gaussian"))

gg <- plot_grid(g1, g2, nrow = 2)
ggsave("output/simulation_settings_5.pdf", gg,
       width = 15, height = 22.5, units = c("in"))


# plot resuls sim6 ####
ll <- read_rds("output/simulation_settings_6-2020-10-19_19_30_08.rds")


# Contour plots
dat <- ll$res
qq <- .9999
md <- "mixture"

dat_plot <- dat %>%
  filter(quantiles_predict == qq, model == md) %>%
  pivot_longer(cols = all_of(c("true", "grf", "erf", "meins", "unconditional")),
               names_to = "method", values_to = "quantile") %>%
  filter((method %in% c("grf", "true", "erf"))) %>%
  mutate(method = factor(method)) %>%
  group_by(X1, X2, method) %>%
  summarise(quantile = mean(quantile))

ggplot(dat_plot, aes(x = X1, y = X2, fill = quantile)) +
  facet_wrap(vars(method)) +
  geom_raster() +
  coord_fixed(expand = FALSE) +
  scale_fill_viridis_c() +
  ggtitle(paste0("Quantile = ", qq))


# Param plots
params <- ll$erf_object %>%
  filter(model == md) %>%
  mutate(true_scale = sigma_mixture(cbind(X1, X2)),
         true_shape = 1 / max(ll$res$df)) %>%
  group_by(id) %>%
  mutate(across(c("true_scale", "scale_param"),
             normalize))

scale_param <- params %>%
  pivot_longer(cols = all_of(c("scale_param", "true_scale")),
               names_to = "method",
               values_to = "scale") %>%
  group_by(X1, X2, method) %>%
  summarise(scale = mean(scale))

shape_param <- params %>%
  pivot_longer(cols = all_of(c("shape_param", "true_shape")),
               names_to = "method",
               values_to = "shape") %>%
  group_by(X1, X2, method) %>%
  summarise(shape = mean(shape))

# scale
ggplot(scale_param, aes(x = X1, y = X2, fill = scale)) +
  facet_wrap(vars(method)) +
  geom_raster() +
  coord_fixed(expand = FALSE) +
  scale_fill_viridis_c()

ggplot(shape_param, aes(x = X1, y = X2, fill = shape)) +
  facet_wrap(vars(method)) +
  geom_raster() +
  coord_fixed(expand = FALSE) +
  scale_fill_viridis_c()


# QQ-plots
ggplot(dat_plot, aes(x = true, y = meins)) +
  geom_point() +
  geom_abline(slope = 1)

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

rm(list = ls())
library(tidyverse)
library(grf)
library(erf)
library(rngtools)
library(randtoolbox)
library(tictoc)
source("simulation_functions.R")


# args
# - simulation_settings
# example: Rscript simulations_parallel.R simulations_settings_1


## set cluster arguments
args = commandArgs(trailingOnly=TRUE)
# args = list("simulation_settings_1", 5)


## set file names
dttime <- gsub(pattern = " |:", x = Sys.time(), replacement = "_")
file_log <- paste("output/", args[[1]], "-", dttime, ".txt", sep = "")
file_rds <- paste("output/", args[[1]],  "-", dttime, ".rds", sep = "")


## set simulation arguments
func <- eval(as.name(args[[1]]))
settings <- set_simulations(simulation_func = func, seed = 42)
sims_args <- settings$simulation_arguments
rm(settings)
m <- NROW(sims_args)


# Loop through all simulations
tic()
ff <- file(file_log, open="wt")
sink(ff)
sink(ff, type="message")
cat("**** Simulation ---", args[[1]] , "**** \n")
ll <- map_dfr(1:m, wrapper_sim, sims_args)
sink(type="message")
sink()
closeAllConnections()
toc()


## collect and save results
ll <- ll %>%
  nest(perf = c(method, quantiles_predict, ise)) %>%
  left_join(sims_args, by = "id")

saveRDS(ll, file = file_rds)

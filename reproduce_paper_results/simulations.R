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

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

settings <- set_simulations(seed = 42)
sims_args <- settings$simulation_arguments
rm(settings)

m <- NROW(sims_args)

# Loop through all simulations
tic()
ff <- file(log_file, open="wt")
sink(ff)
sink(ff, type="message")
cat("**** Simulation **** \n")
ll <- map_dfr(1:m, wrapper_sim, sims_args)
sink(type="message")
sink()
closeAllConnections()
toc()

ll <- ll %>%
  left_join(sims_args, by = "id")

# save results
saveRDS(ll, file = "reproduce_paper_results/output/simulations_092320.rds")

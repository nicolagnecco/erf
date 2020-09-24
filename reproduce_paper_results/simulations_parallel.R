rm(list = ls())
library(tidyverse)
library(grf)
library(erf)
library(foreach)
library(doSNOW)
library(Rmpi)
# source("reproduce_paper_results/simulation_functions.R")


# Simulation arguments
args = commandArgs(trailingOnly=TRUE)

cl <- makeCluster(args[1], type="MPI")
sprintf("start with %s workers", args[1])

numworkers = as.integer(args[1])
nst = 1000/numworkers

# running time
ptm<-proc.time()

clusterExport(cl, list = list())
registerDoSNOW(cl)

# progress bar
# pb <- txtProgressBar(max=100, style=3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress=progress)

# sink(file = args[[2]])
cat("**** Simulation 1 **** \n", file = args[2])
m <- 50
# results <- foreach(i = 1:m, .options.snow=opts) %dopar% {
results <- foreach(i = 1:m) %dopar% {
  cat("Simulation", i, "out of", m, "\n", file = args[2], append = TRUE)
  # cat("Simulation", i, "out of", m, "\n")
  Sys.sleep(.1)
  rnorm(1)
}


# stopCluster(cl)
# mpi.exit()
sink(file = args[[2]], append = TRUE)
print(results)
sink()



# #####
# settings <- set_simulations(seed = 42)
# sims_args <- settings$simulation_arguments
# rm(settings)
#
# # print(proc.time() - ptm)
#
#
# m <- NROW(sims_args)
# m <- 2
#
# # Loop through all simulations
# tic()
# ff <- file(log_file, open="wt")
# sink(ff)
# sink(ff, type="message")
# cat("**** Simulation **** \n")
# ll <- map_dfr(2, wrapper_sim, sims_args)
# sink(type="message")
# sink()
# closeAllConnections()
# toc()
#
# ll <- ll %>%
#   left_join(sims_args, by = "id")
#
# # save results
# saveRDS(ll, file = "reproduce_paper_results/output/simulations_092320.rds")

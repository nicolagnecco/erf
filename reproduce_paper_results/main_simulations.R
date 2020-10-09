rm(list = ls())
library(tidyverse)
library(grf)
library(erf)
library(rngtools)
library(randtoolbox)
library(future)
library(doFuture)
source("simulation_functions.R")


## collect arguments
args <- commandArgs(trailingOnly=TRUE)
# args <- list(simulation = "simulation_settings_3", strategy = "sequential",
#             n_workers = 2)

sim_setting <- args[[1]]
strategy <- args[[2]]
n_workers <- as.double(args[[3]])


## set up file names
dttime <- gsub(pattern = " |:", x = Sys.time(), replacement = "_")
file_log <- "output/progress.txt"
file_rds <- paste("output/", sim_setting,  "-", dttime, ".rds", sep = "")


## set up future strategy
doFuture::registerDoFuture()

if (strategy == "sequential"){
  future::plan(sequential)
} else {
  future::plan(cluster, workers = n_workers)
}


## set simulation arguments
func <- eval(as.name(sim_setting))
sims_args <- func(seed = 42)
m <- NROW(sims_args)


## running time
ptm<-proc.time()
cat("**** Simulation ---", sim_setting , "**** \n", file = file_log)
ll <- foreach(i = 1:m, .combine = bind_rows) %dopar% {
  cat("Simulation", i, "out of", m, "\n", file = file_log, append = TRUE)
  wrapper_sim(i, sims_args)
}
sink(file = file_log, append = TRUE)
print(proc.time() - ptm)
sink()


## collect and save results
ll <- ll %>%
  nest(perf = c(method, quantiles_predict, ise)) %>%
  left_join(sims_args, by = "id")

saveRDS(ll, file = file_rds)

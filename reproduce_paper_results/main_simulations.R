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
args <- list(simulation = "simulation_settings_3", strategy = "sequential",
            workers = 2)


## set up file names
dttime <- gsub(pattern = " |:", x = Sys.time(), replacement = "_")
file_log <- "output/progress.txt"
file_rds <- paste("output/", args[[1]],  "-", dttime, ".rds", sep = "")


## set up future strategy
doFuture::registerDoFuture()

if (args[[2]] == "sequential"){
  future::plan(sequential)
} else {
  future::plan(cluster, workers = args[[3]])
}


## set simulation arguments
func <- eval(as.name(args[[1]]))
sims_args <- func(seed = 42)
m <- NROW(sims_args)


## running time
ptm<-proc.time()
cat("**** Simulation ---", args[[1]] , "**** \n", file = file_log)
ll <- foreach(i = 1:2, .combine = bind_rows) %dopar% {
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

rm(list = ls())
# devtools::install_github("nicolagnecco/erf", auth_token = "f7a9e7c23f4e61c93b17d28a8606c4788c94b073")
library(tidyverse)
library(grf)
library(erf)
library(rngtools)
library(randtoolbox)
library(doParallel)
library(mvtnorm)
source("simulation_functions.R")


## collect arguments
args <- commandArgs(trailingOnly=TRUE)
args <- list(simulation = "simulation_settings_00",
             strategy = c("sequential", "cluster")[1], n_workers = 2)

sim_setting <- args[[1]]
strategy <- args[[2]]
n_workers <- as.double(args[[3]])


## set up file names
dttime <- gsub(pattern = " |:", x = Sys.time(), replacement = "_")
file_log <- "output/progress.txt"
file_rds <- paste("output/", sim_setting,  "-", dttime, ".rds", sep = "")


## set simulation arguments
func <- eval(as.name(sim_setting))
sims_args <- func(seed = 42)
m <- NROW(sims_args)
m <- 1


## set up cluster
if(strategy == "cluster"){
  cl <- parallel::makePSOCKcluster(n_workers)
  registerDoParallel(cl)
  clusterExport(cl, varlist = c("sims_args"))
  clusterEvalQ(cl, {
    library(tidyverse);
    library(grf);
    library(erf);
    library(rngtools);
    library(randtoolbox)
  })
}



## run simulations
ptm<-proc.time()
cat("**** Simulation ---", sim_setting , "**** \n", file = file_log)
ll <- foreach(i = 1:1, .combine = bind_rows) %dopar% {
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

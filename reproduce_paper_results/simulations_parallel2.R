rm(list = ls())
library(tidyverse)
library(grf)
library(erf)
library(rngtools)
library(randtoolbox)
library(tictoc)
library(foreach)
library(doParallel)
source("simulation_functions.R")

# args
# - simulation_settings
# - number of nodes
# example: Rscript simulations_parallel2.R simulations_settings_1 20


## set cluster arguments
args = commandArgs(trailingOnly=TRUE)
# args = list("simulation_settings_1", 20)


cl <- makeCluster(args[[2]], type="PSOCK")
sprintf("start with %s workers", args[[2]])

numworkers = as.integer(args[[2]])
nst = 1000/numworkers


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
m <- 40


## running time
ptm<-proc.time()

clusterExport(cl, list = c("sims_args"))
clusterEvalQ(cl, {
  library(tidyverse);
  library(grf);
  library(erf);
  library(rngtools)
  })
registerDoParallel(cl)

cat("**** Simulation ---", args[[1]] , "**** \n", file = file_log)
ll <- foreach(i = 1:m) %dopar% {
  cat("Simulation", i, "out of", m, "\n", file = file_log, append = TRUE)
  wrapper_sim(i, sims_args)
}


## close connections and show computing time
# stopCluster(cl)
# mpi.exit()
sink(file = file_log, append = TRUE)
print(proc.time() - ptm)
sink()


## collect and save results
ll <- purrr::reduce(ll, bind_rows) %>%
  nest(perf = c(method, quantiles_predict, ise)) %>%
  left_join(sims_args, by = "id")

saveRDS(ll, file = file_rds)

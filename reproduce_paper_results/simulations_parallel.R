rm(list = ls())
library(tidyverse)
library(grf)
library(erf)
library(rngtools)
library(foreach)
library(doSNOW)
library(Rmpi)
source("reproduce_paper_results/simulation_functions.R")


## set cluster arguments
args = commandArgs(trailingOnly=TRUE)
args = list(5, "reproduce_paper_results/output/sims4.txt")

cl <- makeCluster(args[1], type="MPI")
sprintf("start with %s workers", args[1])

numworkers = as.integer(args[1])
nst = 1000/numworkers


## set simulation arguments
settings <- set_simulations(seed = 42)
sims_args <- settings$simulation_arguments
rm(settings)
m <- NROW(sims_args)


## running time
ptm<-proc.time()

clusterExport(cl, list = c("sims_args"))
clusterEvalQ(cl, {
  library(tidyverse);
  library(grf);
  library(erf);
  library(rngtools)
  })
registerDoSNOW(cl)

cat("**** Simulation 1 **** \n", file = args[2])
# m <- 2
ll <- foreach(i = 1:m) %dopar% {
  cat("Simulation", i, "out of", m, "\n", file = args[[2]], append = TRUE)
  wrapper_sim(i, sims_args)
}


## close connections and show computing time
# stopCluster(cl)
# mpi.exit()
sink(file = args[[2]], append = TRUE)
print(proc.time() - ptm)
sink()


## collect and save results
ll <- purrr::reduce(ll, bind_rows)
  left_join(sims_args, by = "id")

saveRDS(ll, file = "reproduce_paper_results/output/simulations_092320.rds")

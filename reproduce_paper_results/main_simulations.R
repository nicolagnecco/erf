#!/usr/bin/env Rscript
# devtools::install_github("nicolagnecco/erf",
#                          auth_token = "f7a9e7c23f4e61c93b17d28a8606c4788c94b073")
library(optparse)
library(tidyverse)
library(grf)
library(erf)
library(rngtools)
library(randtoolbox)
library(doParallel)
library(mvtnorm)
source("simulation_functions.R")


## collect arguments
option_list = list(
  make_option(c("-s", "--simulation_setting"), type="character",
              default=NULL,
              help=paste0("Simulation setting. The name of the function to call ",
                          "to set up the simulation."),
              metavar="character"),
  make_option(c("-p", "--parallel_plan"), type="character", default="sequential",
              help="One of 'sequential' and 'cluster'.", metavar="character"),
  make_option(c("-n", "--n_workers"), type = "integer", default=1,
              help="Number of workers if parallel_plan is 'cluster'.",
              metavar="integer"),
  make_option(c("-t", "--type"), type = "character", default="ise",
              help="One of 'ise' and 'plot'.", metavar="character"),
  make_option(c("-i", "--inspect_erf"), type = "logical", default=FALSE,
              help=paste0("If TRUE, it returns a list that contains simulation results ",
              "and the parameters estimated by erf."), metavar="logical"))

opt_parser <- OptionParser(option_list=option_list);
args <- parse_args(opt_parser);

if (is.null(args$simulation_setting)){
  stop("Please, specify a simulation settings, e.g., simulation_settings_1.")
}

sim_setting <- args$simulation_setting
strategy <- args$parallel_plan
n_workers <- args$n_workers
type <- args$type
inspect_erf <- args$inspect_erf

sim_setting <- "simulation_settings_6"
type <- "plot"
inspect_erf <- TRUE

## set up file names
dttime <- gsub(pattern = " |:", x = Sys.time(), replacement = "_")
file_log <- "output/progress.txt"
file_rds <- paste("output/", sim_setting,  "-", dttime, ".rds", sep = "")


## set simulation arguments
func <- eval(as.name(sim_setting))
sims_args <- func(seed = 42)
m <- NROW(sims_args)


## set up cluster
if(strategy == "cluster"){
  cl <- parallel::makePSOCKcluster(n_workers)
  registerDoParallel(cl)
  clusterExport(cl, varlist = c("sims_args", "type"))
  clusterEvalQ(cl, {
    library(tidyverse);
    library(grf);
    library(erf);
    library(rngtools);
    library(randtoolbox)
  })
}

debug(wrapper_sim)
## run simulations
ptm<-proc.time()
cat("**** Simulation ---", sim_setting , "**** \n", file = file_log)
ll <- foreach(i = 1:m, .combine = bind_rows) %dopar% {
  cat("Simulation", i, "out of", m, "\n", file = file_log, append = TRUE)
  wrapper_sim(i, sims_args, type, inspect_erf)
}
sink(file = file_log, append = TRUE)
print(proc.time() - ptm)
sink()


## collect and save results
if (inspect_erf) {
  res <- ll$res
} else {
  res <- ll
}

if (type == "ise"){
  res <- res %>%
    nest(perf = c(method, quantiles_predict, ise)) %>%
    left_join(sims_args, by = "id")
} else {
  res <- res %>%
    left_join(sims_args %>% select(-quantiles_predict), by = "id")
}

if (inspect_erf){
  ll$res <- res
} else {
  ll <- res
}

saveRDS(ll, file = file_rds)

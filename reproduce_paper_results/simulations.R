source("Tree_functions.R")
library(graphicalExtremes)
library(igraph)
library(matrixcalc)
library(mvtnorm)
library(tidyverse)
library(rngtools)
library(doFuture)
library(here)
library(jsonlite)

# change from here ----
sim_setting <- "sim_study_1"
seed <- 227
strategy <- "parallel" # "sequential" or "parallel"
n_workers <- 2
# to here ----


# process parameters
param <- fromJSON(here("config", paste0(sim_setting, ".json"))) %>% 
  as_tibble() %>%
  set_rng(seed)

if (!has_name(param, "p")){
  param <- param %>% 
    mutate(k = floor(n ** .8),
           p = 1 - k / n)
}


# join with simulation repetitions
sims_args <- expand_grid(sim_id = 1:param$nsim[1],
                         n = param$n) %>% 
  left_join(param, by = "n") %>% 
  arrange(n) %>% 
  rowid_to_column()


# split arguments between fun_args and other_args
other_args_nms <- c("rowid", "sim_id", "nsim", "k", "rng")
fun_args <- sims_args %>% select(!any_of(other_args_nms))
other_args <- sims_args %>% select(any_of(other_args_nms))
m <- nrow(sims_args)


# set up file names
dttime <- gsub(pattern = " |:", x = Sys.time(), replacement = "_")
file_log <- here("output", "progress.txt")
file_rds <- here("output", paste0(sim_setting,  "-", dttime, ".rds"))


# set up cluster
registerDoFuture()
if(strategy == "parallel"){
  plan(multisession, workers = n_workers)
} else {
  plan(sequential)
}


# iterate over simulations
ptm <- proc.time()
cat("**** Simulation ---", sim_setting , "**** \n", file = file_log)
ll <- foreach(i = 1:m, .combine = bind_rows, 
              .options.future = list(scheduling = FALSE),
              .errorhandling = "remove") %dopar% {
                
                # Run simulation
                cat("Simulation", i, "out of", m, "\n", file = file_log, append = TRUE)
                wrapper_sim(i, other_args$rowid[i], sim_study, fun_args)
                
              }
sink(file = file_log, append = TRUE)
cat("\n Time \n")
print(proc.time() - ptm)
sink()


# collect results
ll <- ll %>% 
  nest(perf = c(type, value)) %>% 
  left_join(sims_args, by = "rowid")


# Error log
rowid_errors <- which(!(sims_args$rowid %in% ll$rowid))
if (length(rowid_errors) > 0){
  sink(file = file_log, append = TRUE)
  cat("\nError handling \n")
  cat(paste0("Error occured in iteration with rowid: ", rowid_errors, "\n"))
  sink()
}


# save results
saveRDS(ll, file = file_rds)

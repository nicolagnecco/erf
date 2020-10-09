library(tidyverse)
library(grf)
library(erf)
library(rngtools)
library(randtoolbox)
library(doParallel)
source("simulation_functions.R")

## collect arguments
args <- commandArgs(trailingOnly=TRUE)
# args <- list(strategy = c("sequential", "cluster"), n_workers = 2)

strategy <- args[[1]]
n_workers <- as.double(args[[2]])


## set up file names
dttime <- gsub(pattern = " |:", x = Sys.time(), replacement = "_")
file_log <- "output/progress.txt"
file_rds <- paste("output/", "simulation_grf_weights_", strategy,  "-",
                  dttime, ".rds", sep = "")


## simulate data
set.seed(1290)
n <- 2e3
p <- 40
train <- generate_joint_distribution(n, p, model = "step", distr = "student_t",
                                     df = 2)


## compute weights
x0 <- seq(-1, 1, by = .5)
min.node.size <- c(5, 40, 1e2, n)
honesty <- c(TRUE, FALSE)
sims_args <- expand_grid(x0, min.node.size, honesty)
m <- nrow(sims_args)


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
cat("**** Grf weights ---", strategy, "**** \n", file = file_log)
ll <- foreach(i = 1:m, .combine = bind_rows) %dopar% {
  cat("Simulation", i, "out of", m, "\n", file = file_log, append = TRUE)
  produce_weights_step(i, sims_args, train)
}
sink(file = file_log, append = TRUE)
print(proc.time() - ptm)
sink()


## collect and save results
saveRDS(ll, file = file_rds)

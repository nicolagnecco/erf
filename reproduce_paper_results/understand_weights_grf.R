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
# args <- list(strategy = "sequential", workers = 2)


## set up file names
dttime <- gsub(pattern = " |:", x = Sys.time(), replacement = "_")
file_log <- "output/progress.txt"
file_rds <- paste("output/", "simulation_grf_weights_", args[[1]],  "-",
                  dttime, ".rds", sep = "")


## set up future strategy
doFuture::registerDoFuture()

if (args[[1]] == "sequential"){
  future::plan(sequential)
} else {
  future::plan(cluster, workers = args[[2]])
}


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
params <- expand_grid(x0, min.node.size, honesty)
m <- nrow(params)

ptm<-proc.time()
cat("**** Grf weights ---", args[[1]], "**** \n", file = file_log)
ll <- foreach(i = 1:m, .combine = bind_rows) %dopar% {
  cat("Simulation", i, "out of", m, "\n", file = file_log, append = TRUE)
  produce_weights_step(i, params, train)
}
sink(file = file_log, append = TRUE)
print(proc.time() - ptm)
sink()

## collect and save results
saveRDS(ll, file = file_rds)

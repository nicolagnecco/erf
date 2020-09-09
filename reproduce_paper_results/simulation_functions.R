square_loss <- function(y, y_hat){
  ## numeric_vector numeric_vector -> numeric
  ## produce square loss
  (y - y_hat) ^ 2
}

quantile_loss <- function(y, y_hat, alpha){
  ## numeric_vector numeric_vector numeric(0, 1) -> numeric
  ## produce quantile loss
  if (y > y_hat){
    alpha * abs(y - y_hat)
  } else {
    (1 - alpha) * abs(y - y_hat)
  }
}

generate_joint_distribution <- function(n, p,
                                        model = c("gaussian", "student_t"),
                                        scale = 2, df){
  ## integer integer character numeric integer -> list
  ## generate n iid observations of (X, Y), where X is p-dimensional predictor
  ## and Y is the response. Y has conditional distribution equal to
  ## model = "gaussian" or "student_t" with a certain scale and
  ## with df degree of freedom.
  ## Returns a list with:
  ## - X, nxp matrix, p-dimensional predictor
  ## - Y, vector with n elements, response variable

  model <- match.arg(model)
  X <- matrix(runif(n * p, min = -1, max = 1), n, p)

  switch(model,
         "gaussian" = {
           Y = ifelse(X[,1] < 0, rnorm(n, 0, 1), rnorm(n, 0, scale))
         },
         "student_t" = {
           Y = ifelse(X[,1] < 0, rt(n, df = df),  scale * rt(n, df = df))
         })

  return(list(X = X, Y = Y))

}

generate_conditional_distribution <- function(n, p, x,
                                              model = c("gaussian", "student_t"),
                                              df){
  ## integer integer numeric_vector character integer -> numeric_matrix
  ## generate n iid observations of the conditional response Y at values x.
  ## Y has conditional distribution equal to model = "gaussian" or "student_t"
  ## with df degree of freedom.
  ## Returns Y, vector with n elements, response variable

  # !!!

  return(matrix(NA, nrow = n, ncol = 1))
}

generate_theoretical_quantiles <- function(alpha, x, model, df, scale){
  ## numeric_vector numeric_vector character integer numeric -> numeric_matrix
  ## produce conditional alpha-quantiles at the x values for model = "gaussian"
  ## or "student_t" with df degree of freedom and relative scale

  ntest <- length(x)
  nalphas <- length(alpha)
  ntest_negative <- length(which(x < 0))
  ntest_nonnegative <- ntest - ntest_negative

  if(model == "gaussian"){
    q1function <-  function(p) qnorm(p, mean=0, sd=1)
    q2function <-  function(p) qnorm(p, mean=0, sd=scale)
  }
  if(model == "student_t"){
    q1function <- function(p) qt(p, df = df)
    q2function <- function(p) scale * qt(p, df = df)
  }


  q_true <- matrix(NA, nrow = ntest, ncol = nalphas)
  q_true[which(x < 0), ] <- matrix(q1function(alpha), nrow = ntest_negative,
                                 ncol = nalphas, byrow = TRUE)
  q_true[which(x >= 0), ] <- matrix(q2function(alpha), nrow = ntest_nonnegative,
                                 ncol = nalphas, byrow = TRUE)

  return(q_true)
}

simulation_settings <- function(){
  ## void -> tibble
  ## returns a tibble with simulation settings

  ## base parameter values
  n0 <- 2e3
  p0 <- 40
  scale0 <- 2
  num.trees0 <- 2e3
  min.node.size0 <- 5
  honesty0 <- TRUE
  threshold0 <- 0.8
  out_of_bag0 <- FALSE

  ## other parameter values
  ## general
  nexp <- 1:20
  n <- c(n0, 500, 1000)
  p <- c(p0, 10, 20)
  ntest <- 100
  model <- c("gaussian", "student_t")
  df <- c(1.5, 2.5)
  scale <- c(scale0, 4)

  ## fit
  num.trees <- c(num.trees0, 500, 1000)
  quantiles_fit <- c(0.1, 0.5, 0.9)
  min.node.size <- c(5, 2, 10, 20)
  honesty <- c(honesty0, FALSE)

  ## predict
  quantiles_predict <- c(.9, .99, .999, .9995)
  threshold <- c(threshold0, .5, .9)
  out_of_bag <- c(out_of_bag0, TRUE)


  ## create parameter grid
  ## tibble 1
  tbl1 <- expand_grid(n, p, scale, num.trees, min.node.size, honesty, threshold,
                      out_of_bag) %>%
    filter(n %in% n0 + p %in% p0 + scale %in% scale0 +
             num.trees %in% num.trees0 + min.node.size %in% min.node.size0 +
             honesty %in% honesty0 + threshold %in% threshold0 +
             out_of_bag %in% out_of_bag0 >= 7) %>%
    mutate(id = 1)

  tbl2 <- expand_grid(nexp, ntest) %>%
    mutate(quantiles_fit = list(quantiles_fit),
           quantiles_predict = list(quantiles_predict),
           id = 1)

  tbl3 <- expand_grid(model, df) %>%
    mutate(df = if_else(model == "gaussian", NaN, df)) %>%
    mutate(id = 1) %>%
    distinct()

  my_args <- full_join(tbl2, tbl3) %>%
    full_join(tbl1) %>%
    select(-id) %>%
    rowwise() %>%
    mutate(id = cur_group_id()) %>%
    select(id, everything())

  return(my_args)
}

set_simulations <- function(experiment_ids=1:NROW(simulation_settings()),
                            seed){
  ## numeric_vector character_vector integer -> list
  ## prepares the settings for the simulations
  ## INPUTS:
  ## - experiment_ids: a numeric vector that specifies the rows of the tibble generated
  ## by calling the function simulation_settings().
  ## - seed: an integer seed that determines the outcome of the simulation.
  ##
  ## RETURNS:
  ## The function returns a list made of:
  ## - simulation_arguments: a tibble generated by the function simulation_settings(),
  ## where only the rows in experiments_ids are kept.
  ## NOTE:
  ## The simulations are fully repeatable. That is, even after running the full
  ## simulation, you can repeat it over a subset of arguments
  ## This is possible because this function assigns to each simulation run
  ## a unique random seed. These random seeds are generated with
  ## L'Ecuyer RNG method and are independent of each other.

  # set simulation options
  simulation_arguments <- simulation_settings()
  m <- NROW(simulation_arguments)
  if (any(!(experiment_ids %in% 1:m))){
    stop(paste("Argument experiment_ids must contain integers between 1 and ",
               m, ".", sep = ""))
  }

  experiment_ids <- sort(unique(experiment_ids))

  # create independent RNG streams with L'Ecuyer method
  rng <- RNGseq(m, seed = seed)

  # select RNG streams for simulations
  sims_rng_stream <- rng[1:m]
  simulation_arguments$rng <- sims_rng_stream

  # filter simulations
  simulation_arguments <- simulation_arguments %>%
    filter(id %in% experiment_ids)

  # return list
  list(simulation_arguments=simulation_arguments)

}

wrapper_sim <- function(i, sims_args){
  m <- nrow(sims_args)
  cat("Simulation", i, "out of", m, "\n")


  # set current simulation variables
  id <- sims_args$id[i]
  nexp <- sims_args$nexp[i]
  ntest <- sims_args$ntest[i]
  quantiles_fit <- sims_args$quantiles_fit[[i]]
  quantiles_predict <- sims_args$quantiles_predict[[i]]
  model <- sims_args$model[i]
  df <- sims_args$df[i]
  n <- sims_args$n[i]
  p <- sims_args$p[i]
  scale <- sims_args$scale[i]
  num.trees <- sims_args$num.trees[i]
  min.node.size <- sims_args$min.node.size[i]
  honesty <- sims_args$honesty[i]
  threshold <- sims_args$threshold[i]
  out_of_bag <- sims_args$out_of_bag[i]


  # generate training data
  rng_sims <- sims_args$rng[[i]]
  rngtools::setRNG(rng_sims)
  dat <- generate_joint_distribution(n = n, p = p, model = model, df = df)

  # generate test data
  x_test <- matrix(0, nrow = ntest, ncol = p)
  x_test[, 1] <- seq(-1, 1, length.out = ntest)

  # fit quantile regression function
  fit_grf <- quantile_forest(dat$X, dat$Y, quantiles = quantiles_fit,
                             num.trees = num.trees,
                             min.node.size = min.node.size, honesty = honesty)

  # predict quantile regression function
  predictions_grf <- predict(fit_grf, x_test, quantiles = quantiles_predict)
  predictions_erf <- predict_erf(fit_grf, quantiles = quantiles_predict,
                                 threshold = threshold,
                                 newdata = x_test, model_assessment = FALSE,
                                 Y.test = NULL,
                                 out_of_bag = out_of_bag)$predictions
  predictions_true <- generate_theoretical_quantiles(alpha = quantiles_predict,
                                                     x = x_test[, 1],
                                                     model = model, df = df,
                                                     scale = scale)


  # collect results
  tb_erf <- tibble(id = id,
                   x = x_test[, 1],
                   method = "erf",
                   predictions = matrix2list(predictions_erf))

  tb_grf <- tibble(id = id,
                   x = x_test[, 1],
                   method = "grf",
                   predictions = matrix2list(predictions_grf))

  tb_true <- tibble(id = id,
                    x = x_test[, 1],
                    method = "true",
                    predictions = matrix2list(predictions_true))


  res <- bind_rows(tb_true, tb_grf, tb_erf)

  # return value
  return(res)
}

matrix2list <- function(mat){
  ## numeric_matrix -> list
  ## produces a list with elements corresponding to rows of mat
  split(mat, rep(1:nrow(mat), times = ncol(mat)))
}

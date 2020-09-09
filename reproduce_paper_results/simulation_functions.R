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

generate_theoretical_quantiles <- function(alpha, x, model, df){
  ## numeric numeric_vector character integer -> numeric_matrix
  ## produce conditional alpha-quantile at the x values for model = "gaussian"
  ## or "student_t" with df degree of freedom

  ntest <- length(x)
  ntest_negative <- length(which(x < 0))
  ntest_nonnegative <- ntest - ntest_negative

  if(model == "gaussian"){
    q1function <-  function(p) qnorm(p, mean=0, sd=1)
    q2function <-  function(p) qnorm(p, mean=0, sd=2)
  }
  if(model == "student_t"){
    q1function <- function(p) qt(p, df = df)
    q2function <- function(p) 2 * qt(p, df = df)
  }


  q_true <- matrix(NA, nrow = ntest, ncol = 1)
  q_true[which(x < 0)] <- matrix(q1function(alpha), nrow = ntest_negative,
                                 ncol = 1)
  q_true[which(x >= 0)] <- matrix(q2function(alpha), nrow = ntest_nonnegative,
                                 ncol = 1)

  return(q_true)
}

simulation_settings <- function(){
  ## void -> tibble
  ## returns a tibble with simulation settings

  # list of arguments
  ## general
  # nexp: 20
  # n: 2000 (500, 1000)
  # p: 40 (20, 10)
  # ntest: 100
  # model, df: gaussian, student_1.5, student_2.5
  # scale: 2 (4)
  #
  ## fit
  # num.trees: 2000 (500, 1000)
  # quantiles_fit: c(0.1, 0.5, 0.9)
  # min.node.size: 5 (2, 10, 20)
  # honesty: TRUE (FALSE)
  #
  ## predict
  # quantiles_predict: .8, .99, .999, .9995
  # threshold: 0.8 (.6, .9)
  # out_of_bag: FALSE (TRUE)

  ## base parameter values
  n0 <- 2e3
  p0 <- 40
  scale0 <- 2
  num.trees0 <- 2e3
  min.node.sizec0 <- 5
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
  min.node.sizec <- c(5, 2, 10, 20)
  honesty <- c(honesty0, FALSE)

  ## predict
  quantiles_predict <- c(.8, .99, .999, .9995)
  threshold <- c(threshold0, .5, .9)
  out_of_bag <- c(out_of_bag0, TRUE)


  ## create parameter grid
  ## tibble 1
  tbl1 <- expand_grid(n, p, scale, num.trees,min.node.sizec, honesty, threshold,
                      out_of_bag) %>%
    filter(n %in% n0 + p %in% p0 + scale %in% scale0 +
             num.trees %in% num.trees0 + min.node.sizec %in% min.node.sizec0 +
             honesty %in% honesty0 + threshold %in% threshold0 +
             out_of_bag %in% out_of_bag0 >= 7)

  3 * 3 * 2 * 3 * 4 * 2 * 3 * 2 # cardinality of cartesian product
  1 + 2 + 2 + 1 + 2 + 3 + 1 + 2 + 1 # cardinality of set of interest

  AA <- expand_grid(nexp = nexp, n = n, p =  p, scale =scale)

  AA2 <-  AA %>%
    filter((n == n[1]) + (p == p[1]) + (scale == scale[1]) == 2)

  view(AA2)
  id <- AA$n %in% n[1] + AA$p %in% p[1] + AA$scale %in% scale[1] == 3
  AA[id, ]

  # basic example
  A <- c("a1", "a2", "a3")
  B <- c("b1", "b2")
  A1 <- A[1:2]
  B1 <- B[1]
  A2 <- A[3]
  B2 <- B[2]

  A_cross_B <- expand_grid(A, B)
  A2_cross_B2 <- expand_grid(A2, B2) %>%
    rename(A = A2, B = B2)

  # solution 1
  setdiff(A_cross_B, A2_cross_B2)

  # solution 2
  A_cross_B %>%
    filter(A %in% A1 | B %in% B1)

  # create tibble with base parameters (define a base set of parameters)
  # and then change one parameter at a time
  # one possibility to do that is to create the cartesian product of the hyperparameters,
  # and then only retain the ones that are in the base set
  quantiles_predict <- c(.8, .99, .999, .9995)
  model <- c("gaussian", "student_t")
  df <- c(1.5, 2.5)

  tb1 <- expand_grid(nexp, quantiles_predict) %>%
    arrange(desc(quantiles_predict)) %>%
    mutate(id = 1)

  tb2 <- expand_grid(model, df) %>%
    mutate(df = if_else(model == "gaussian", NaN, df)) %>%
    mutate(id = 1) %>%
    distinct()

  my_args <- full_join(tb1, tb2) %>%
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
  nexp <- sims_args$nexp[i]
  quantiles_predict <- sims_args$quantiles_predict[i]
  model <- sims_args$model[i]
  df <- sims_args$df[i]

  # generate training data
  rng_sims <- sims_args$rng[[i]]
  rngtools::setRNG(rng_sims)
  dat <- generate_joint_distribution(n = n, p = p, model = model, df = df)

  # generate test data
  x_test <- matrix(0, nrow = ntest, ncol = p)
  x_test[, 1] <- seq(-1, 1, length.out = ntest)

  # fit quantile regression function
  fit_grf <- quantile_forest(dat$X, dat$Y, quantiles = c(0.1, 0.5, 0.9))

  # predict quantile regression function
  predictions_grf <- predict(fit_grf, x_test, quantiles = quantiles_predict)
  predictions_erf <- predict_erf(fit_grf, quantiles = quantiles_predict,
                                 threshold = 0.8,
                                 newdata = x_test, model_assessment = FALSE,
                                 Y.test = NULL,
                                 out_of_bag = FALSE)$predictions
  predictions_true <- generate_theoretical_quantiles(alpha = quantiles_predict,
                                                     x = x_test[, 1],
                                                     model = model, df = df)


  # collect results
  tb1 <- tibble(nexp = nexp,
                model = model,
                quantiles_predict = quantiles_predict,
                df = df)

  tb_erf <- tibble(nexp = nexp,
                   x = x_test[, 1],
                   method = "erf",
                   predictions = predictions_erf[, 1])

  tb_grf <- tibble(nexp = nexp,
                   x = x_test[, 1],
                   method = "grf",
                   predictions = predictions_grf[, 1])

  tb_true <- tibble(nexp = nexp,
                    x = x_test[, 1],
                    method = "true",
                    predictions = predictions_true[, 1])


  res <- bind_rows(tb_true, tb_grf, tb_erf) %>%
    left_join(tb1, by = "nexp")


  # return value
  return(res)
}

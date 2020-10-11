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

predict_unconditional_quantiles <- function(threshold, alpha, Y, ntest){
  ## numeric numeric_vector integer-> numeric_matrix
  ## predict high unconditional quantiles

  # helper
  q_GPD <- function(p, p0, t_x0, sigma, xi){
    ## numeric(0, 1) numeric(0, 1) numeric_matrix numeric_vector
    ## numeric_vector -> numeric_vector
    ## produce the estimated extreme quantiles of GPD

    (((1-p)/(1-p0))^{-xi} - 1) * (sigma / xi) + t_x0
  }

  # body
  p0 <- threshold
  t0 <- quantile(Y, p0)
  pars <- ismev::gpd.fit(Y, t0, show = FALSE)$mle
  sigma <- pars[1]
  xi <- pars[2]

  q_hat <- q_GPD(alpha, p0, t0, sigma, xi)
  return(matrix(q_hat, nrow = ntest,
                ncol = length(alpha), byrow = T))

}

generate_joint_distribution <- function(n, p,
                                        model = c("step", "periodic", "gaussian"),
                                        distr = c("gaussian", "student_t"),
                                        df){
  ## integer (x2) character (x2) integer -> list
  ## generate n iid observations of (X, Y), where X is p-dimensional predictor
  ## and Y is the response following the given model with the
  ## given distribution.
  ## Returns a list with:
  ## - X, nxp matrix, p-dimensional predictor
  ## - Y, vector with n elements, response variable

  # helpers
  step_model <- function(X, distr, df){
    ## numeric_matrix charachter integer -> numeric_vector
    ## produce response Y for the step model

    n <- nrow(X)
    p <- ncol(X)

    switch(distr,
           "gaussian" = {
             Y_tilde <- rnorm(n)
           },
           "student_t" = {
             Y_tilde <- rt(n, df = df)
           })

    sigma_x <- 1 + 1 * (X[, 1] > 0)
    Y <- sigma_x * Y_tilde

    return(Y)
  }

  periodic_model <- function(X, distr){
    ## numeric_matrix charachter -> numeric_vector
    ## produce response Y for the periodic model

    n <- nrow(X)
    p <- ncol(X)

    switch(distr,
           "gaussian" = {
             Y_tilde <- rnorm(n)
           },
           "student_t" = {
             df_x <- 3 - cos(X[, 1] * 3/2 * pi)
             Y_tilde <- rt(n, df = df_x)
           })

    sigma_x <- 3/2 + 1/2 * cos((X[, 1]^2 + X[, 2]^2) * 3/2 * pi)
    Y <- sigma_x * Y_tilde

    return(Y)
  }

  gaussian_model <- function(X, distr){
    ## numeric_matrix charachter -> numeric_vector
    ## produce response Y for the gaussian model

    n <- nrow(X)
    p <- ncol(X)

    switch(distr,
           "gaussian" = {
             Y_tilde <- rnorm(n)
           },
           "student_t" = {
             df_x <- 7 * (1 + exp(4 * X[, 1] + 1.2))^(-1) + 3
             Y_tilde <- rt(n, df = df_x)
           })

    sigma_x <- 1 + 6 * mvtnorm::dmvnorm(X[, c(1, 2)],
                                        sigma = rbind(c(1, .9), c(.9, 1)))
    Y <- sigma_x * Y_tilde

    return(Y)
  }

  # body
  model <- match.arg(model)
  distr <- match.arg(distr)
  X <- matrix(runif(n * p, min = -1, max = 1), n, p)

  switch(model,
         "step" = {
           Y <- step_model(X, distr, df)
         },
         "periodic" = {
           Y <- periodic_model(X, distr)
         },
         "gaussian" = {
           Y <- gaussian_model(X, distr)
         })

  return(list(X = X, Y = Y))

}

generate_theoretical_quantiles <- function(alpha, X,
                                           model = c("step", "periodic", "gaussian"),
                                           distr = c("gaussian", "student_t"),
                                           df){
  ## numeric_vector numeric_matrix character (x2) integer -> numeric_matrix
  ## produce theoretical quantiles for the given model and distribution
  ## for the different observations (rows of X)

  # helpers
  step_model_quantiles <- function(alpha, X, distr, df){
    ## numeric_vector numeric_matrix charachter integer -> numeric_vector
    ## produce theoretical quantiles Y for the step model

    n <- nrow(X)
    p <- ncol(X)

    switch(distr,
           "gaussian" = {
             q_tilde <- qnorm(alpha)
           },
           "student_t" = {
             q_tilde <- qt(alpha, df = df)
           })

    sigma_x <- 1 + 1 * (X[, 1] > 0)
    q <- as.matrix(sigma_x) %*% t(q_tilde)

    return(q)
  }

  periodic_model_quantiles <- function(alpha, X, distr){
    ## numeric_vector numeric_matrix charachter -> numeric_vector
    ## produce theoretical quantiles for the periodic model

    n <- nrow(X)
    p <- length(alpha)

    switch(distr,
           "gaussian" = {
             q_tilde <- matrix(qnorm(alpha), nrow = n, ncol = p, byrow = TRUE)
           },
           "student_t" = {
             df_x <- 3 - cos(X[, 1] * 3/2 * pi)
             q_tilde <- sapply(alpha, qt, df = df_x)
           })

    sigma_x <- 3/2 + 1/2 * cos((X[, 1]^2 + X[, 2]^2) * 3/2 * pi)
    q <- sigma_x * q_tilde

    return(q)
  }

  gaussian_model_quantiles <- function(alpha, X, distr){
    ## numeric_vector numeric_matrix charachter -> numeric_vector
    ## produce theoretical quantiles for the gaussian model

    n <- nrow(X)
    p <- length(alpha)

    switch(distr,
           "gaussian" = {
             q_tilde <- matrix(qnorm(alpha), nrow = n, ncol = p, byrow = TRUE)
           },
           "student_t" = {
             df_x <- 7 * (1 + exp(4 * X[, 1] + 1.2))^(-1) + 3
             q_tilde <- sapply(alpha, qt, df = df_x)
           })

    sigma_x <- 1 + 6 * mvtnorm::dmvnorm(X[, c(1, 2)],
                                        sigma = rbind(c(1, .9), c(.9, 1)))
    q <- sigma_x * q_tilde

    return(q)
  }

  # body
  model <- match.arg(model)
  distr <- match.arg(distr)
  n <- nrow(X)
  p <- ncol(X)


  switch(model,
         "step" = {
           quantiles <- step_model_quantiles(alpha, X, distr, df)
         },
         "periodic" = {
           quantiles <- periodic_model_quantiles(alpha, X, distr)
         },
         "gaussian" = {
           quantiles <- gaussian_model_quantiles(alpha, X, distr)
         })

  return(quantiles)

}

simulation_settings_0 <- function(seed){
  ## integer -> tibble
  ## simulation setting as in gbex paper

  ## base parameter values
  n0 <- 2e3
  p0 <- 40
  num.trees0 <- 2e3
  min.node.size0 <- 5
  honesty0 <- TRUE
  threshold0 <- 0.8
  out_of_bag0 <- FALSE

  ## other parameter values
  ## general
  nexp <- 1:1e3
  n <- c(n0)
  p <- c(p0)
  ntest <- 1e4
  model <- c("step")
  distr <- c("student_t")
  df <- c(4)

  ## fit
  num.trees <- c(num.trees0)
  quantiles_fit <- c(0.1, 0.5, 0.9)
  min.node.size <- c(min.node.size0)
  honesty <- c(honesty0)

  ## predict
  quantiles_predict <- c(.99, .995, .999, .9995, .9999)
  threshold <- c(threshold0)
  out_of_bag <- c(out_of_bag0)


  ## create parameter grid
  ## tibble 1
  tbl1 <- expand_grid(n, p, num.trees, min.node.size, honesty, threshold,
                      out_of_bag) %>%
    filter(n %in% n0
           + p %in% p0
           + num.trees %in% num.trees0
           + min.node.size %in% min.node.size0
           + honesty %in% honesty0
           + threshold %in% threshold0
           + out_of_bag %in% out_of_bag0  >= 6) %>%
    mutate(id = 1)

  tbl2 <- expand_grid(nexp, ntest, model) %>%
    mutate(quantiles_fit = list(quantiles_fit),
           quantiles_predict = list(quantiles_predict),
           id = 1)

  tbl3 <- expand_grid(distr, df) %>%
    mutate(df = if_else(distr == "gaussian", NaN, df)) %>%
    mutate(id = 1) %>%
    distinct()

  my_args <- full_join(tbl2, tbl3, by = "id") %>%
    full_join(tbl1, by = "id") %>%
    select(-id) %>%
    rowwise() %>%
    mutate(id = cur_group_id()) %>%
    select(id, everything()) %>%
    set_rng(seed)

  return(my_args)
}

simulation_settings_1 <- function(seed){
  ## integer -> tibble
  ## parameter sweep with step function

  ## base parameter values
  n0 <- 2e3
  p0 <- 40
  num.trees0 <- 2e3
  min.node.size0 <- 5
  honesty0 <- TRUE
  threshold0 <- 0.8
  out_of_bag0 <- FALSE

  ## other parameter values
  ## general
  nexp <- 1:1e2
  n <- c(n0, 500, 1000)
  p <- c(p0, 10, 20)
  ntest <- 1e3
  model <- c("step")
  distr <- c("gaussian", "student_t")
  df <- c(2, 3, 4)

  ## fit
  num.trees <- c(num.trees0, 500, 3000, 5000)
  quantiles_fit <- c(0.1, 0.5, 0.9)
  min.node.size <- c(min.node.size0, 20, 40)
  honesty <- c(honesty0, FALSE)

  ## predict
  quantiles_predict <- c(.99, .995, .999, .9995, .9999)
  threshold <- c(threshold0, .5, .9)
  out_of_bag <- c(out_of_bag0, TRUE)


  ## create parameter grid
  ## tibble 1
  tbl1 <- expand_grid(n, p, num.trees, min.node.size, honesty, threshold,
                      out_of_bag) %>%
    filter(n %in% n0
           + p %in% p0
           + num.trees %in% num.trees0
           + min.node.size %in% min.node.size0
           + honesty %in% honesty0
           + threshold %in% threshold0
           + out_of_bag %in% out_of_bag0  >= 6) %>%
    mutate(id = 1)

  tbl2 <- expand_grid(nexp, ntest, model) %>%
    mutate(quantiles_fit = list(quantiles_fit),
           quantiles_predict = list(quantiles_predict),
           id = 1)

  tbl3 <- expand_grid(distr, df) %>%
    mutate(df = if_else(distr == "gaussian", NaN, df)) %>%
    mutate(id = 1) %>%
    distinct()

  my_args <- full_join(tbl2, tbl3, by = "id") %>%
    full_join(tbl1, by = "id") %>%
    select(-id) %>%
    rowwise() %>%
    mutate(id = cur_group_id()) %>%
    select(id, everything()) %>%
    set_rng(seed)

  return(my_args)
}

simulation_settings_2 <- function(seed){
  ## integer -> tibble
  ## parameter sweep with periodic function

  ## base parameter values
  n0 <- 2e3
  p0 <- 10
  num.trees0 <- 2e3
  min.node.size0 <- 5
  honesty0 <- TRUE
  threshold0 <- 0.8
  out_of_bag0 <- FALSE

  ## other parameter values
  ## general
  nexp <- 1:1e2
  n <- c(n0, 500, 1000)
  p <- c(p0, 40, 20)
  ntest <- 1e3
  model <- c("periodic")
  distr <- c("gaussian", "student_t")
  df <- c(2, 3, 4)

  ## fit
  num.trees <- c(num.trees0, 500, 3000, 5000)
  quantiles_fit <- c(0.1, 0.5, 0.9)
  min.node.size <- c(min.node.size0, 20, 40)
  honesty <- c(honesty0, FALSE)

  ## predict
  quantiles_predict <- c(.99, .995, .999, .9995, .9999)
  threshold <- c(threshold0, .5, .9)
  out_of_bag <- c(out_of_bag0, TRUE)


  ## create parameter grid
  ## tibble 1
  tbl1 <- expand_grid(n, p, num.trees, min.node.size, honesty, threshold,
                      out_of_bag) %>%
    filter(n %in% n0
           + p %in% p0
           + num.trees %in% num.trees0
           + min.node.size %in% min.node.size0
           + honesty %in% honesty0
           + threshold %in% threshold0
           + out_of_bag %in% out_of_bag0  >= 6) %>%
    mutate(id = 1)

  tbl2 <- expand_grid(nexp, ntest, model) %>%
    mutate(quantiles_fit = list(quantiles_fit),
           quantiles_predict = list(quantiles_predict),
           id = 1)

  tbl3 <- expand_grid(distr, df) %>%
    mutate(df = if_else(distr == "gaussian", NaN, df)) %>%
    mutate(id = 1) %>%
    distinct()

  my_args <- full_join(tbl2, tbl3, by = "id") %>%
    full_join(tbl1, by = "id") %>%
    select(-id) %>%
    rowwise() %>%
    mutate(id = cur_group_id()) %>%
    select(id, everything()) %>%
    set_rng(seed)

  return(my_args)
}

simulation_settings_3 <- function(seed){
  ## void -> tibble
  ## check impact of different methods of generating test data

  ## base parameter values
  n0 <- 2e3
  p0 <- 40
  num.trees0 <- 2e3
  min.node.size0 <- 100
  honesty0 <- TRUE
  threshold0 <- 0.8
  out_of_bag0 <- TRUE
  test_data0 <- "halton"

  ## other parameter values
  ## general
  nexp <- 1:1e2
  n <- c(n0)
  p <- c(p0)
  ntest <- 1e3
  model <- c("step")
  distr <- c("student_t")
  df <- c(4)

  ## fit
  num.trees <- c(num.trees0)
  quantiles_fit <- c(0.1, 0.5, 0.9)
  min.node.size <- c(min.node.size0)
  honesty <- c(honesty0)

  ## predict
  quantiles_predict <- c(.99, .995, .999, .9995, .9999)
  threshold <- c(threshold0)
  out_of_bag <- c(out_of_bag0)
  test_data <- c(test_data0, "uniform", "zero")

  ## create parameter grid
  ## tibble 1
  tbl1 <- expand_grid(n, p, num.trees, min.node.size, honesty, threshold,
                      out_of_bag) %>%
    mutate(id = 1)

  tbl2 <- expand_grid(nexp, ntest, model) %>%
    mutate(quantiles_fit = list(quantiles_fit),
           quantiles_predict = list(quantiles_predict),
           id = 1)

  tbl3 <- expand_grid(distr, df) %>%
    mutate(df = if_else(distr == "gaussian", NaN, df)) %>%
    mutate(id = 1) %>%
    distinct()

  tbl4 <- full_join(tbl2, tbl3, by = "id") %>%
    full_join(tbl1, by = "id") %>%
    set_rng(seed)

  tbl5 <- tibble(test_data = test_data, id = 1)

  my_args <- tbl4 %>%
    left_join(tbl5, by = "id") %>%
    select(-id) %>%
    rowwise() %>%
    mutate(id = cur_group_id()) %>%
    select(id, everything())

  return(my_args)

}

simulation_settings_4 <- function(seed){
  ## void -> tibble
  ## check impact of different methods of generating test data

  ## base parameter values
  n0 <- 2e3
  p0 <- 40
  num.trees0 <- 2e3
  min.node.size0 <- 100
  honesty0 <- TRUE
  threshold0 <- 0.8
  out_of_bag0 <- TRUE

  ## other parameter values
  ## general
  nexp <- 1:1e2
  n <- c(n0)
  p <- c(p0)
  ntest <- 1e3
  model <- c("step")
  distr <- c("student_t")
  df <- c(4)

  ## fit
  num.trees <- c(num.trees0)
  quantiles_fit <- c(0.1, 0.5, 0.9)
  min.node.size <- c(min.node.size0, 5, 40, n0)
  honesty <- c(honesty0, FALSE)

  ## predict
  quantiles_predict <- c(.99, .995, .999, .9995, .9999)
  threshold <- c(threshold0)
  out_of_bag <- c(out_of_bag0)

  ## create parameter grid
  ## tibble 1
  tbl1 <- expand_grid(n, p, num.trees, min.node.size, honesty, threshold,
                      out_of_bag) %>%
    mutate(id = 1)

  tbl2 <- expand_grid(nexp, ntest, model) %>%
    mutate(quantiles_fit = list(quantiles_fit),
           quantiles_predict = list(quantiles_predict),
           id = 1)

  tbl3 <- expand_grid(distr, df) %>%
    mutate(df = if_else(distr == "gaussian", NaN, df)) %>%
    mutate(id = 1) %>%
    distinct()

  my_args <- full_join(tbl2, tbl3, by = "id") %>%
    full_join(tbl1, by = "id") %>%
    select(-id) %>%
    rowwise() %>%
    mutate(id = cur_group_id()) %>%
    select(id, everything()) %>%
    set_rng(seed)

  return(my_args)

}

set_rng <- function(tbl, seed){
  ## tibble integer -> tibble
  ## adds to tbl a column with seeds to generate independent streams of random
  ## numbers.
  ##
  ## INPUTS:
  ## - tbl: a tibble where the columns contain the parameter settings and the
  ## rows contain the simulation runs.
  ##
  ## RETURNS:
  ## The function returns tbl appending a column with seeds used to generate
  ## independent streams of random numbers.
  ##
  ## NOTE:
  ## The simulations are fully repeatable. That is, even after running the full
  ## simulation, you can repeat it over a subset of arguments
  ## This is possible because this function assigns to each simulation run
  ## a unique random seed. These seeds are generated with
  ## L'Ecuyer RNG method and generate independent streams of random numbers.

  m <- nrow(tbl)

  # create independent RNG streams with L'Ecuyer method
  rng <- RNGseq(m, seed = seed)

  # add RNG streams to tbl
  tbl$rng <- rng

  # return updated tbl
  return(tbl)
}

wrapper_sim <- function(i, sims_args){
  ## integer tibble character -> tibble
  ## run simulations over arguments of row i in sims_args

  m <- nrow(sims_args)
  cat("Simulation", i, "out of", m, "\n")


  # set current simulation variables
  id <- sims_args$id[i]
  nexp <- sims_args$nexp[i]
  ntest <- sims_args$ntest[i]
  quantiles_fit <- sims_args$quantiles_fit[[i]]
  quantiles_predict <- sims_args$quantiles_predict[[i]]
  model <- sims_args$model[i]
  distr <- sims_args$distr[i]
  df <- sims_args$df[i]
  n <- sims_args$n[i]
  p <- sims_args$p[i]

  num.trees <- sims_args$num.trees[i]
  min.node.size <- sims_args$min.node.size[i]
  honesty <- sims_args$honesty[i]
  threshold <- sims_args$threshold[i]
  out_of_bag <- sims_args$out_of_bag[i]
  if(sims_args %>% has_name("test_data")){
    test_data <- sims_args[["test_data"]][i]
  } else {
    test_data <- "halton"
  }


  # generate training data
  rng_sims <- sims_args$rng[[i]]
  rngtools::setRNG(rng_sims)
  dat <- generate_joint_distribution(n = n, p = p, model = model,
                                     distr = distr, df = df)

  # fit models
  # fit quantile regression function w/ grf
  fit_grf <- quantile_forest(dat$X, dat$Y, quantiles = quantiles_fit,
                             num.trees = num.trees,
                             min.node.size = min.node.size, honesty = honesty)


  # fit quantile regression functions w/ meinshausen
  fit_meins <- quantile_forest(dat$X, dat$Y, quantiles = quantiles_fit,
                               num.trees = num.trees,
                               regression.splitting = TRUE,
                               min.node.size = min.node.size,
                               honesty = honesty)


  # generate test data (#??? maybe temp option?)
  if (test_data == "halton"){
    X_test <- randtoolbox::halton(ntest, p) * 2 - 1

  } else if (test_data == "uniform"){
    X_test <- matrix(runif(ntest * p, min = -1, max = 1),
                     nrow = ntest, ncol = p)

  } else if (test_data == "zero"){
    X_test <- matrix(0, nrow = ntest, ncol = p)
    X_test[, 1] <- seq(-1, 1, length.out = ntest)
  }


  # predict models
  # predict quantile regression functions w/ grf
  predictions_grf <- predict(fit_grf, X_test, quantiles = quantiles_predict)

  # predict quantile regression functions w/ erf
  predictions_erf <- predict_erf(fit_grf, quantiles = quantiles_predict,
                                 threshold = threshold,
                                 newdata = X_test, model_assessment = FALSE,
                                 Y.test = NULL,
                                 out_of_bag = out_of_bag)$predictions

  # predict quantile regression functions w/ meinshausen
  predictions_meins <- predict(fit_meins, X_test,
                               quantiles = quantiles_predict)


  # predict unconditional quantile
  predictions_unconditional <- predict_unconditional_quantiles(threshold = threshold,
                                                           alpha = quantiles_predict,
                                                           Y = dat$Y,
                                                           ntest = ntest)

  # predict true quantile regression functions
  predictions_true <- generate_theoretical_quantiles(alpha = quantiles_predict,
                                                     X = X_test,
                                                     model = model,
                                                     distr = distr, df = df)

  # collect results
  tb_erf <- tibble(id = id,
                   method = "erf",
                   predictions = matrix2list(predictions_erf)) %>%
    rowid_to_column()

  tb_grf <- tibble(id = id,
                   method = "grf",
                   predictions = matrix2list(predictions_grf)) %>%
    rowid_to_column()

  tb_true <- tibble(id = id,
                    method = "true",
                    predictions = matrix2list(predictions_true)) %>%
    rowid_to_column()

  tb_unconditional <- tibble(id = id,
                        method = "unconditional",
                        predictions = matrix2list(predictions_unconditional)) %>%
    rowid_to_column()

  tb_meins <- tibble(id = id,
                     method = "meins",
                     predictions = matrix2list(predictions_meins)) %>%
    rowid_to_column()

  method <- c("erf", "grf", "meins", "unconditional")


  res <- bind_rows(tb_true, tb_grf, tb_erf, tb_unconditional, tb_meins) %>%
    mutate(quantiles_predict = list(quantiles_predict)) %>%
    unnest(cols = c(quantiles_predict, predictions)) %>%
    pivot_wider(names_from = "method",
                values_from = "predictions") %>%
    mutate(across(all_of(method), function(x){(x - true)^2})) %>%
    select(-"true") %>%
    pivot_longer(cols = all_of(method),
                 names_to = "method", values_to = "se") %>%
    group_by(id, method, quantiles_predict) %>%
    summarise(ise = mean(se))

  # return value
  return(res)
}

matrix2list <- function(mat){
  ## numeric_matrix -> list
  ## produces a list with elements corresponding to rows of mat
  split(mat, rep(1:nrow(mat), times = ncol(mat)))
}

produce_weights_step <- function(i, params, train){
  ## integer tibble list -> tibble
  ## produce tibble with weights, training predictors and params

  m <- nrow(params)
  n <- nrow(train$X)
  p <- ncol(train$X)

  x0 <- params$x0[i]
  x0_mat <- matrix(c(x0,
                     rep(0, p-1)),
                   nrow = 1, ncol = p)
  min.node.size <- params$min.node.size[i]
  honesty <- params$honesty[i]


  fit_grf <- grf::quantile_forest(train$X, train$Y,
                                  min.node.size = min.node.size,
                                  honesty = honesty)

  weights <- as.matrix(grf::get_sample_weights(fit_grf, newdata = x0_mat,
                                               num.threads = NULL))[1, ]

  res <- tibble(X1 = train$X[, 1], weights = weights, x0 = x0,
                min.node.size = min.node.size, honesty = honesty)

  return(res)
}

wrapper_sim_weights_gpd <- function(i, sims_args){
  ## integer tibble character -> tibble
  ## run simulations over arguments of row i in sims_args

  m <- nrow(sims_args)
  cat("Simulation", i, "out of", m, "\n")


  # set current simulation variables
  id <- sims_args$id[i]
  nexp <- sims_args$nexp[i]
  ntest <- sims_args$ntest[i]
  quantiles_fit <- sims_args$quantiles_fit[[i]]
  quantiles_predict <- sims_args$quantiles_predict[[i]]
  model <- sims_args$model[i]
  distr <- sims_args$distr[i]
  df <- sims_args$df[i]
  n <- sims_args$n[i]
  p <- sims_args$p[i]

  num.trees <- sims_args$num.trees[i]
  min.node.size <- sims_args$min.node.size[i]
  honesty <- sims_args$honesty[i]
  threshold <- sims_args$threshold[i]
  out_of_bag <- sims_args$out_of_bag[i]
  if(sims_args %>% has_name("test_data")){
    test_data <- sims_args[["test_data"]][i]
  } else {
    test_data <- "halton"
  }


  # generate training data
  rng_sims <- sims_args$rng[[i]]
  rngtools::setRNG(rng_sims)
  dat <- generate_joint_distribution(n = n, p = p, model = model,
                                     distr = distr, df = df)

  # fit models
  # fit quantile regression function w/ grf
  fit_grf <- quantile_forest(dat$X, dat$Y, quantiles = quantiles_fit,
                             num.trees = num.trees,
                             min.node.size = min.node.size, honesty = honesty)

  # generate test data (#??? maybe temp option?)
  if (test_data == "halton"){
    X_test <- randtoolbox::halton(ntest, p) * 2 - 1

  } else if (test_data == "uniform"){
    X_test <- matrix(runif(ntest * p, min = -1, max = 1),
                     nrow = ntest, ncol = p)

  } else if (test_data == "zero"){
    X_test <- matrix(0, nrow = ntest, ncol = p)
    X_test[, 1] <- seq(-1, 1, length.out = ntest)
  }


  # predict models
  # predict quantile regression functions w/ erf
  predictions_erf <- predict_erf(fit_grf, quantiles = quantiles_predict,
                                 threshold = threshold,
                                 newdata = X_test, model_assessment = FALSE,
                                 Y.test = NULL,
                                 out_of_bag = out_of_bag)$predictions

  # predict quantile regression functions w/ erf with exact weights
  wi_x0 <- get_step_weights(dat$X, X_test)
  t_xi <- get_step_intermediate_thres(dat$X, X_test)

  predictions_erf_true_wgts <- erf:::predict_erf_internal(fit_grf, quantiles = quantiles_predict,
                                 threshold = threshold,
                                 newdata = X_test, model_assessment = FALSE,
                                 Y.test = NULL,
                                 out_of_bag = out_of_bag,
                                 wi_x0 = wi_x0, t_xi = t_xi)$predictions

  # predict true quantile regression functions
  predictions_true <- generate_theoretical_quantiles(alpha = quantiles_predict,
                                                     X = X_test,
                                                     model = model,
                                                     distr = distr, df = df)

  # collect results
  tb_erf <- tibble(id = id,
                   method = "erf",
                   predictions = matrix2list(predictions_erf)) %>%
    rowid_to_column()

  tb_erf_wgts <- tibble(id = id,
                   method = "erf_true_weights",
                   predictions = matrix2list(predictions_erf_true_wgts)) %>%
    rowid_to_column()

  tb_true <- tibble(id = id,
                    method = "true",
                    predictions = matrix2list(predictions_true)) %>%
    rowid_to_column()

  method <- c("erf", "erf_true_weights")


  res <- bind_rows(tb_true, tb_erf, tb_erf_wgts) %>%
    mutate(quantiles_predict = list(quantiles_predict)) %>%
    unnest(cols = c(quantiles_predict, predictions)) %>%
    pivot_wider(names_from = "method",
                values_from = "predictions") %>%
    mutate(across(all_of(method), function(x){(x - true)^2})) %>%
    select(-"true") %>%
    pivot_longer(cols = all_of(method),
                 names_to = "method", values_to = "se") %>%
    group_by(id, method, quantiles_predict) %>%
    summarise(ise = mean(se))

  # return value
  return(res)
}

get_step_weights <- function(x_train, x_test){
  ## numeric_matrix (x2) -> numeric_matrix
  ## produce exact weights for step function

  # helper
  get_step_weights_x0 <- function(x0, x_train){
    wgts_x0 <- if (x0 < 0){
      1 / sum(x_train[, 1] < 0) * (x_train[, 1] < 0)
    } else {
      1 / sum(x_train[, 1] >=0) * (x_train[, 1] >=0)
    }

    return(wgts_x0)
  }

  # body
  n <- nrow(x_train)
  p <- ncol(x_train)
  ntest <- nrow(x_test)

  wgts_list <- lapply(1:ntest,  function(i){
    get_step_weights_x0(x_test[i, 1], x_train)
  })

  matrix(unlist(wgts_list), nrow = ntest, byrow = TRUE)

}


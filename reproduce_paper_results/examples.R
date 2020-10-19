# predict method
n <- 2e3
p <- 10
ntest <- 1e2
model <- "step"
distr <- "gaussian"
df <- 1
test_method <- "halton"
quantiles_predict <- c(.99, .999)
threshold <- .8

dat <- generate_joint_distribution(n = n, p = p, model = model,
                                   distr = distr, df = df)

X_test <- generate_test_data(ntest, p, test_method)

# fit models
# GRF
fit_grf <- quantile_forest(dat$X, dat$Y)

# predict
arg_list <- list(object = fit_grf, quantiles = quantiles_predict,
                 threshold = threshold,  newdata = X_test)
pred_object <- do.call(predict_erf, arg_list)

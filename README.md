
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Extremal Random Forests

<!-- badges: start -->

[![R build
status](https://github.com/nicolagnecco/erf/workflows/R-CMD-check/badge.svg)](https://github.com/nicolagnecco/erf/actions)
<!-- badges: end -->

The package `erf` implements the extremal random forests (ERF), an
algorithm to predict extreme conditional quantiles in large dimensions.
For more details see Gnecco, Terefe, and Engelke (2023,
https://arxiv.org/abs/2201.12865).

## Installation

<!-- You can install the released version of erf from [CRAN](https://CRAN.R-project.org) with: -->

``` r
install.packages("erf")
```

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nicolagnecco/erf")
```

## Example

This basic example shows how to fit and predict conditional quantiles
with `erf`.

``` r
library(erf)
library(ggplot2)
library(dplyr)

# Function to model scale
scale_step <- function(X) {
  ## numeric_vecotr -> numeric_vector
  ## produce scale function: scale(X) = step function

  sigma_x <- 1 + 1 * (X > 0)

  return(sigma_x)
}

# Generate data
set.seed(42)

n <- 2000
p <- 10
X <- matrix(runif(n * p, min = -1, max = 1), n, p)
Y <- scale_step(X[, 1]) * rnorm(n)

# Fit ERF
fit_erf <- erf(X, Y, intermediate_quantile = 0.8)

# Predict ERF
quantiles <- c(0.9, 0.99)
pred_erf <- predict(fit_erf, newdata = X, quantiles = quantiles)

true_quantiles <- matrix(rep(qnorm(quantiles), n), 
                         ncol = length(quantiles),
                         byrow = TRUE) * scale_step(X[, 1])

# Plot results
my_palette <- list(
  "red" = "#D55E00",
  "blue" = "#0072B2"
)

ggplot() +
  geom_point(aes(x = X[, 1], y = Y), alpha = .5, col = "grey") +
  geom_point(aes(x = X[, 1], y = pred_erf[, 2]), alpha = .5, 
             col = my_palette$blue) +
  geom_line(aes(x = X[, 1], y = true_quantiles[, 2]), col = my_palette$red, 
            linetype = "dashed", size = 1) +
  theme_bw()
#> Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
#> ℹ Please use `linewidth` instead.
```

<img src="man/figures/README-example1-1.png" width="100%" />

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-merg2020" class="csl-entry">

Gnecco, Nicola, Edossa Merga Terefe, and Sebastian Engelke. 2023.
“Extremal Random Forests.” <https://arxiv.org/abs/2201.12865>.

</div>

</div>

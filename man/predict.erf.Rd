% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.erf.R
\name{predict.erf}
\alias{predict.erf}
\title{Predict an extremal random forest (ERF)}
\usage{
\method{predict}{erf}(object, newdata = NULL, quantiles = c(0.95, 0.99), ...)
}
\arguments{
\item{object}{Fitted \code{erf} object.}

\item{newdata}{Numeric matrix or \code{data.frame} of test predictor values
at which predictions are to be made. If \code{NULL}, predictions are made on the
training data instead.
For further information see \code{\link[grf:quantile_forest]{grf::quantile_forest()}}.
Default is \code{NULL}.}

\item{quantiles}{Numeric vector of quantile levels at which estimates are
to be made.
Default is \code{c(0.95, 0.99)}.}

\item{...}{Dots. Currently ignored.}
}
\value{
Numeric matrix with predictions for each test point (rows)
at the desired quantile levels (columns).
}
\description{
Makes prediction using a fitted \code{erf} object.
}

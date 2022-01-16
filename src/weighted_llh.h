#ifndef __UTILITIES__
#define __UTILITIES__

double weighted_llh(const arma::vec & par,
                    const arma::vec & data,
                    const arma::vec & weights,
                    double lambda,
                    double xi_prior,
                    double intermediate_quantile);

#endif // __UTILITIES__

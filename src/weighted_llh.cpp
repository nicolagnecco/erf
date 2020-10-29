#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export(rng = FALSE)]]
double weighted_llh(const arma::vec & par,
                    const arma::vec & data,
                    const arma::vec & weights,
                    double lambda,
                    double xi_prior){

  if(par[0] <= 0){
    return 1e6;
  } else {
    if(arma::min(1 + data / (par[0]/par[1])) <= 0){
      return 1e6;
    } else{
     return sum(weights %
                (log(par[0]) + log(1 + data / (par[0]/par[1]))
                   / (1 / (1 + 1/par[1])))) / data.size()
      + lambda * pow(par[1] - xi_prior, 2);
    }

  }

}

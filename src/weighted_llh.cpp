#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
double weighted_llh(const arma::vec & par,
                    const arma::vec & data,
                    const arma::vec & weights,
                    double lambda,
                    double xi_prior,
                    double intermediate_quantile){

  if(par[0] <= 0){
    return 1e6;
  } else {
    if(arma::min(1 + data / (par[0]/par[1])) <= 0){
      return 1e6;
    } else{
     return sum(weights %
                (log(par[0]) + log(1 + data / (par[0]/par[1]))
                   / (1 / (1 + 1/par[1])))) / (1 - intermediate_quantile)
      + lambda * pow(par[1] - xi_prior, 2);
    }

  }

}

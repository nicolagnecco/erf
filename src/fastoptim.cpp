#include <cmath>  // std::pow

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <roptim.h>
// [[Rcpp::depends(roptim)]]

#include "weighted_llh.h"

using namespace roptim;

class F : public Functor {
public:
  arma::vec data;
  arma::vec weights;
  double lambda;
  double xi_prior;
  double intermediate_quantile;
  arma::mat P;
  arma::vec b;
  double operator()(const arma::vec &par) override {
    return weighted_llh(par, data, weights, lambda,
                        xi_prior, intermediate_quantile);
  }
};


arma::rowvec get_vmin_i(arma::vec& init_pars,
                        const arma::vec & data,
                        const arma::vec & weights,
                        double lambda,
                        double xi_prior,
                        double intermediate_quantile) {
  F optimand;
  optimand.data = data;
  optimand.weights = weights;
  optimand.lambda = lambda;
  optimand.xi_prior = xi_prior;
  optimand.intermediate_quantile = intermediate_quantile;

  // "Nelder-Mead":
  Roptim<F> opt1;
  opt1.minimize(optimand, init_pars);
  // opt1.print();
  return opt1.par().t();
}


// [[Rcpp::export(rng = FALSE)]]
arma::mat predict_gpd_params_cpp(arma::vec& init_pars,
                                 const arma::vec & data,
                                 const arma::mat & weights_mat,
                                 double lambda,
                                 double xi_prior,
                                 double intermediate_quantile) {

  // define variables
  int i, n;

  n = weights_mat.n_cols;
  arma::mat gpd_pars = arma::mat(n, 2, arma::fill::zeros);


  // predict gpd parameters
  for (i = 0; i < n; i++){
    gpd_pars.row(i) = get_vmin_i(
      init_pars, data, weights_mat.col(i),
      lambda, xi_prior, intermediate_quantile
    );

    // Rcpp::Rcout << "The value of v : " <<
    //   get_vmin_i(init_pars,
    //              data, weights_mat.col(i),
    //              lambda, xi_prior, intermediate_quantile) << "\n";
  }

  return gpd_pars;
}

#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <roptim.h>
// [[Rcpp::depends(roptim)]]

using namespace roptim;

// class weighted_llh2 : public Functor {
// public:
//   double operator()(const arma::vec & data,
//                   const arma::vec & weights,
//                   const arma::vec & pars) override {
//                     if(pars[0] <= 0){
//                       return 1e6;
//                     } else {
//                       if(arma::min(1 + data / (pars[0]/pars[1])) <= 0){
//                         return 1e6;
//                       } else{
//                         return sum(weights %
//                                    (log(pars[0]) + log(1 + data / (pars[0]/pars[1]))
//                                       / (1 / (1 + 1/pars[1]))));
//                       }
//
//                     }
//                   }
// };
//
//
// // [[Rcpp::export(rng = FALSE)]]
// arma::vec rosen_bfgs(const arma::vec & data,
//                      const arma::vec & weights,
//                      const arma::vec & pars) {
//   weighted_llh2 rb;
//   Roptim<weighted_llh2> opt("Nelder-Mead");
//
//
//   arma::vec x = {-1.2, 1};
//   opt.minimize(rb, data, weights, pars);
//
//   return opt.par();
// }

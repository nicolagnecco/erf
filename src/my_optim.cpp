#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <roptim.h>
// [[Rcpp::depends(roptim)]]

using namespace roptim;

class weighted_llh2 : public Functor {
public:
  double operator()(const arma::vec & par,
                  const arma::vec & data,
                  const arma::vec & weights) override {
                    if(par[0] <= 0){
                      return 1e6;
                    } else {
                      if(arma::min(1 + data / (par[0]/par[1])) <= 0){
                        return 1e6;
                      } else{
                        return sum(weights %
                                   (log(par[0]) + log(1 + data / (par[0]/par[1]))
                                      / (1 / (1 + 1/par[1]))));
                      }

                    }
                  }
};


// [[Rcpp::export(rng = FALSE)]]
arma::vec optim_cpp(const arma::vec & par,
                     const arma::vec & data,
                     const arma::vec & weights) {
  weighted_llh2 rb;
  Roptim<weighted_llh2> opt("Nelder-Mead");

  opt.minimize(rb, par, data, weights);

  return opt.par();
}

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix raceSimC(NumericVector ratings,int nsim) {
   int nr = ratings.size();
   double p;
   NumericVector r;
   IntegerMatrix out(nr,nsim);
   std::fill( out.begin(), out.end(), 1);
   
   for (int i = 0; i < nsim; ++i){
     for (int j = 0; j < nr-1; ++j){
       for (int k = j+1; k < nr; ++k){
         p = 1 / (pow(10,-(ratings[j] - ratings[k]) / 400) + 1);
         r = runif(1);
         if (r[0] <= p){
           out(k,i) += 1;
         }else{
           out(j,i) += 1;
         }
       }
     }
   }
   return out;
}

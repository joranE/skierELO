#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

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

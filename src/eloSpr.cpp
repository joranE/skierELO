#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector eloSprC(NumericVector rating, IntegerVector raceCount, double K, double P, int provisionalN) {
  int n = rating.size();
  NumericVector out = clone(rating);
  double W_e, e, delta_a, delta_b;
  
  for (int j1 = 0; j1 < n-1; ++j1){
    for (int j2 = j1 + 1; j2 < n; ++j2){
      e = -1 * (rating[j1] - rating[j2]) / 400;
      W_e = 1 / (pow(10,e) + 1);
      
      delta_a = (K / (n-1)) * log(j2 - j1 + 2) * (1 - W_e);
      delta_b = -1 * delta_a;
      
      if (raceCount[j1] < provisionalN && raceCount[j2] < provisionalN){
        delta_a = P * delta_a;
        delta_b = P * delta_b;
      }
      if (raceCount[j1] < provisionalN && raceCount[j2] >= provisionalN){
        delta_a = P * delta_a;
        delta_b = 0;
      }
      if (raceCount[j1] >= provisionalN && raceCount[j2] < provisionalN){
        delta_a = 0;
        delta_b = P * delta_b;
      }
      
      out[j1] += delta_a;
      out[j2] += delta_b;
    }
  }
  return out;
}

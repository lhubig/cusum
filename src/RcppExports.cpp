// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// gscusum
NumericMatrix gscusum(NumericMatrix& input_outcomes, double failure_probability, double odds_multiplier, double limit, NumericVector& quantiles, int max_num_shuffles, int seed);
RcppExport SEXP _cusum_gscusum(SEXP input_outcomesSEXP, SEXP failure_probabilitySEXP, SEXP odds_multiplierSEXP, SEXP limitSEXP, SEXP quantilesSEXP, SEXP max_num_shufflesSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type input_outcomes(input_outcomesSEXP);
    Rcpp::traits::input_parameter< double >::type failure_probability(failure_probabilitySEXP);
    Rcpp::traits::input_parameter< double >::type odds_multiplier(odds_multiplierSEXP);
    Rcpp::traits::input_parameter< double >::type limit(limitSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type quantiles(quantilesSEXP);
    Rcpp::traits::input_parameter< int >::type max_num_shuffles(max_num_shufflesSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(gscusum(input_outcomes, failure_probability, odds_multiplier, limit, quantiles, max_num_shuffles, seed));
    return rcpp_result_gen;
END_RCPP
}
// ragscusum
NumericMatrix ragscusum(NumericMatrix& input_ra_outcomes, double limit, NumericVector& quantiles, int max_num_shuffles, int seed);
RcppExport SEXP _cusum_ragscusum(SEXP input_ra_outcomesSEXP, SEXP limitSEXP, SEXP quantilesSEXP, SEXP max_num_shufflesSEXP, SEXP seedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix& >::type input_ra_outcomes(input_ra_outcomesSEXP);
    Rcpp::traits::input_parameter< double >::type limit(limitSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type quantiles(quantilesSEXP);
    Rcpp::traits::input_parameter< int >::type max_num_shuffles(max_num_shufflesSEXP);
    Rcpp::traits::input_parameter< int >::type seed(seedSEXP);
    rcpp_result_gen = Rcpp::wrap(ragscusum(input_ra_outcomes, limit, quantiles, max_num_shuffles, seed));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_cusum_gscusum", (DL_FUNC) &_cusum_gscusum, 7},
    {"_cusum_ragscusum", (DL_FUNC) &_cusum_ragscusum, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_cusum(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

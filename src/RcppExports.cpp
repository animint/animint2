#include <Rcpp.h>
using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// common_value_for_group_subset_cpp
List common_value_for_group_subset_cpp(List value_lists);
RcppExport SEXP _animint2_common_value_for_group_subset_cpp(SEXP value_listsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type value_lists(value_listsSEXP);
    rcpp_result_gen = Rcpp::wrap(common_value_for_group_subset_cpp(value_lists));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_animint2_common_value_for_group_subset_cpp", (DL_FUNC) &_animint2_common_value_for_group_subset_cpp, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_animint2(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

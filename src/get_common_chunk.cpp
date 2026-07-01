#include <Rcpp.h>
#include <algorithm>
#include <vector>

using namespace Rcpp;

namespace {

bool is_na_at(SEXP v, int i) {
  switch (TYPEOF(v)) {
  case REALSXP: return ISNA(REAL(v)[i]);
  case INTSXP: return INTEGER(v)[i] == NA_INTEGER;
  case LGLSXP: return LOGICAL(v)[i] == NA_LOGICAL;
  case STRSXP: return STRING_ELT(v, i) == NA_STRING;
  default: return false;
  }
}

bool eq_at(SEXP a, int ia, SEXP b, int ib) {
  if (TYPEOF(a) != TYPEOF(b)) return false;
  switch (TYPEOF(a)) {
  case REALSXP: {
    double da = REAL(a)[ia], db = REAL(b)[ib];
    return (ISNA(da) && ISNA(db)) || da == db;
  }
  case INTSXP: {
    int da = INTEGER(a)[ia], db = INTEGER(b)[ib];
    return (da == NA_INTEGER && db == NA_INTEGER) || da == db;
  }
  case LGLSXP: {
    int da = LOGICAL(a)[ia], db = LOGICAL(b)[ib];
    return (da == NA_LOGICAL && db == NA_LOGICAL) || da == db;
  }
  case STRSXP:
    return STRING_ELT(a, ia) == STRING_ELT(b, ib);
  default:
    return false;
  }
}

SEXP scalar_at(SEXP v, int i) {
  switch (TYPEOF(v)) {
  case REALSXP: return Rf_ScalarReal(REAL(v)[i]);
  case INTSXP: return Rf_ScalarInteger(INTEGER(v)[i]);
  case LGLSXP: return Rf_ScalarLogical(LOGICAL(v)[i]);
  case STRSXP: return Rf_ScalarString(STRING_ELT(v, i));
  default: return R_NilValue;
  }
}

bool all_same(const std::vector<SEXP>& xs) {
  if (xs.empty()) return false;
  for (size_t k = 1; k < xs.size(); ++k) {
    if (!eq_at(xs[0], 0, xs[k], 0)) return false;
  }
  return true;
}

SEXP scalars_to_vector(const std::vector<SEXP>& xs) {
  if (xs.empty()) return R_NilValue;
  const int n = static_cast<int>(xs.size());
  switch (TYPEOF(xs[0])) {
  case REALSXP: {
    NumericVector out(n);
    for (int i = 0; i < n; ++i) out[i] = REAL(xs[i])[0];
    return out;
  }
  case INTSXP: {
    IntegerVector out(n);
    for (int i = 0; i < n; ++i) out[i] = INTEGER(xs[i])[0];
    return out;
  }
  case LGLSXP: {
    LogicalVector out(n);
    for (int i = 0; i < n; ++i) out[i] = LOGICAL(xs[i])[0];
    return out;
  }
  case STRSXP: {
    CharacterVector out(n);
    for (int i = 0; i < n; ++i) out[i] = STRING_ELT(xs[i], 0);
    return out;
  }
  default:
    return xs[0];
  }
}

List wrap_common(SEXP x) {
  return List::create(x);
}

}  // namespace

// [[Rcpp::export]]
List common_value_for_group_subset_cpp(List value_lists) {
  const int nchunks = value_lists.size();
  if (nchunks == 0) {
    return List::create(Named("common") = List(), Named("is.common") = false);
  }
  std::vector<SEXP> vecs(nchunks);
  std::vector<int> lvec(nchunks);
  for (int c = 0; c < nchunks; ++c) {
    vecs[c] = value_lists[c];
    lvec[c] = Rf_length(vecs[c]);
  }
  if (lvec[0] > 0 && std::equal(lvec.begin() + 1, lvec.end(), lvec.begin())) {
    const int gs = lvec[0];
    std::vector<SEXP> min_na(gs, R_NilValue);
    for (int r = 0; r < gs; ++r) {
      for (int c = 0; c < nchunks; ++c) {
        if (!is_na_at(vecs[c], r)) {
          min_na[r] = scalar_at(vecs[c], r);
          break;
        }
      }
      if (min_na[r] == R_NilValue) min_na[r] = scalar_at(vecs[0], r);
    }
    const bool ref_scalar = all_same(min_na);
    const SEXP ref0 = min_na[0];
    bool is_common = true;
    for (int c = 0; c < nchunks && is_common; ++c) {
      for (int r = 0; r < gs; ++r) {
        if (is_na_at(vecs[c], r)) continue;
        SEXP ref = ref_scalar ? ref0 : min_na[r];
        if (!eq_at(vecs[c], r, ref, 0)) {
          is_common = false;
          break;
        }
      }
    }
    if (ref_scalar) {
      return List::create(
        Named("common") = wrap_common(ref0),
        Named("is.common") = is_common
      );
    }
    return List::create(
      Named("common") = wrap_common(scalars_to_vector(min_na)),
      Named("is.common") = is_common
    );
  }
  std::vector<SEXP> flat;
  for (int c = 0; c < nchunks; ++c) {
    for (int r = 0; r < lvec[c]; ++r) flat.push_back(scalar_at(vecs[c], r));
  }
  if (!flat.empty() && all_same(flat)) {
    return List::create(
      Named("common") = wrap_common(flat[0]),
      Named("is.common") = true
    );
  }
  return List::create(Named("common") = List(), Named("is.common") = false);
}

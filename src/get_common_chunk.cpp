#include <Rcpp.h>
#include <algorithm>
#include <numeric>
#include <vector>

using namespace Rcpp;

namespace {

struct CellRef {
  int chunk;
  int row;
};

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

bool eq_refs(const std::vector<SEXP>& vecs, CellRef a, CellRef b) {
  return eq_at(vecs[a.chunk], a.row, vecs[b.chunk], b.row);
}

bool all_same_refs(const std::vector<SEXP>& vecs, const std::vector<CellRef>& refs) {
  if (refs.empty()) return false;
  for (size_t k = 1; k < refs.size(); ++k) {
    if (!eq_refs(vecs, refs[0], refs[k])) return false;
  }
  return true;
}

/* Build an atomic vector by reading cells from original chunk vectors.
   No intermediate Rf_Scalar* SEXPs are stored, so nothing needs PROTECT
   across allocations (Rcpp vectors protect themselves). */
SEXP refs_to_vector(const std::vector<SEXP>& vecs, const std::vector<CellRef>& refs) {
  if (refs.empty()) return R_NilValue;
  const int n = static_cast<int>(refs.size());
  SEXP first = vecs[refs[0].chunk];
  switch (TYPEOF(first)) {
  case REALSXP: {
    NumericVector out(n);
    for (int i = 0; i < n; ++i) {
      out[i] = REAL(vecs[refs[i].chunk])[refs[i].row];
    }
    return out;
  }
  case INTSXP: {
    IntegerVector out(n);
    for (int i = 0; i < n; ++i) {
      out[i] = INTEGER(vecs[refs[i].chunk])[refs[i].row];
    }
    return out;
  }
  case LGLSXP: {
    LogicalVector out(n);
    for (int i = 0; i < n; ++i) {
      out[i] = LOGICAL(vecs[refs[i].chunk])[refs[i].row];
    }
    return out;
  }
  case STRSXP: {
    CharacterVector out(n);
    for (int i = 0; i < n; ++i) {
      out[i] = STRING_ELT(vecs[refs[i].chunk], refs[i].row);
    }
    return out;
  }
  default:
    return R_NilValue;
  }
}

/* Allocate one scalar and immediately wrap it in a List so Rcpp's
   protection covers the SEXP before any further allocation. */
List wrap_common_scalar(const std::vector<SEXP>& vecs, CellRef ref) {
  SEXP v = vecs[ref.chunk];
  switch (TYPEOF(v)) {
  case REALSXP:
    return List::create(Rf_ScalarReal(REAL(v)[ref.row]));
  case INTSXP:
    return List::create(Rf_ScalarInteger(INTEGER(v)[ref.row]));
  case LGLSXP:
    return List::create(Rf_ScalarLogical(LOGICAL(v)[ref.row]));
  case STRSXP:
    return List::create(Rf_ScalarString(STRING_ELT(v, ref.row)));
  default:
    return List::create(R_NilValue);
  }
}

List wrap_common_vector(SEXP x) {
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
    /* Store (chunk, row) indices into the original vectors instead of
       allocating Rf_Scalar* SEXPs. Those SEXPs would need PROTECT because
       later allocations can trigger GC. Index refs need no protection. */
    std::vector<CellRef> min_na(gs);
    for (int r = 0; r < gs; ++r) {
      min_na[r].chunk = 0;
      min_na[r].row = r;
      for (int c = 0; c < nchunks; ++c) {
        if (!is_na_at(vecs[c], r)) {
          min_na[r].chunk = c;
          min_na[r].row = r;
          break;
        }
      }
    }
    const bool ref_scalar = all_same_refs(vecs, min_na);
    const CellRef ref0 = min_na[0];
    bool is_common = true;
    for (int c = 0; c < nchunks && is_common; ++c) {
      for (int r = 0; r < gs; ++r) {
        if (is_na_at(vecs[c], r)) continue;
        CellRef ref = ref_scalar ? ref0 : min_na[r];
        if (!eq_at(vecs[c], r, vecs[ref.chunk], ref.row)) {
          is_common = false;
          break;
        }
      }
    }
    if (ref_scalar) {
      return List::create(
        Named("common") = wrap_common_scalar(vecs, ref0),
        Named("is.common") = is_common
      );
    }
    return List::create(
      Named("common") = wrap_common_vector(refs_to_vector(vecs, min_na)),
      Named("is.common") = is_common
    );
  }
  std::vector<CellRef> flat;
  flat.reserve(static_cast<size_t>(std::accumulate(lvec.begin(), lvec.end(), 0)));
  for (int c = 0; c < nchunks; ++c) {
    for (int r = 0; r < lvec[c]; ++r) {
      CellRef ref;
      ref.chunk = c;
      ref.row = r;
      flat.push_back(ref);
    }
  }
  if (!flat.empty() && all_same_refs(vecs, flat)) {
    return List::create(
      Named("common") = wrap_common_scalar(vecs, flat[0]),
      Named("is.common") = true
    );
  }
  return List::create(Named("common") = List(), Named("is.common") = false);
}

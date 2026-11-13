#include <Rcpp.h>
#include <cstring>
using namespace Rcpp;

// One column, one group. value_lists has one atomic vector per chunk.
// This is the inner compare from getCommonChunk():
//   same length  -> first non-NA in each row, then collapse if that is
//                   one unique value; is.common ignores NA cells
//   else         -> common only when every value is identical

// NA matches NA, so length(unique(x)) == 1 still holds for an all-NA vector.
template <int RTYPE>
bool same_value(typename traits::storage_type<RTYPE>::type a,
                typename traits::storage_type<RTYPE>::type b) {
  if (Vector<RTYPE>::is_na(a) || Vector<RTYPE>::is_na(b)) {
    return Vector<RTYPE>::is_na(a) && Vector<RTYPE>::is_na(b);
  }
  return a == b;
}

template <>
bool same_value<STRSXP>(SEXP a, SEXP b) {
  if (a == NA_STRING || b == NA_STRING) return a == b;
  return std::strcmp(CHAR(a), CHAR(b)) == 0;
}

template <int RTYPE>
List common_impl(const List& chunks) {
  typedef Vector<RTYPE> Vec;
  typedef typename traits::storage_type<RTYPE>::type T;
  int n_chunks = chunks.size();
  std::vector<Vec> cols;
  cols.reserve(n_chunks);
  int n0 = Rf_length(chunks[0]);
  bool same_len = true;
  for (int j = 0; j < n_chunks; j++) {
    cols.push_back(Vec(static_cast<SEXP>(chunks[j])));
    if (cols[j].size() != n0) same_len = false;
  }
  if (!same_len) {
    bool have = false;
    T first = Vec::get_na();
    for (int j = 0; j < n_chunks; j++) {
      for (int i = 0; i < cols[j].size(); i++) {
        T value = cols[j][i];
        if (!have) {
          first = value;
          have = true;
        } else if (!same_value<RTYPE>(value, first)) {
          return List::create(
            Named("common") = List::create(),
            Named("is.common") = false);
        }
      }
    }
    Vec scalar(1);
    scalar[0] = first;
    return List::create(
      Named("common") = List::create(scalar),
      Named("is.common") = true);
  }
  int nrow = n0;
  Vec first(nrow);
  std::vector<char> seen(nrow, 0);
  for (int i = 0; i < nrow; i++) first[i] = Vec::get_na();
  for (int j = 0; j < n_chunks; j++) {
    for (int i = 0; i < nrow; i++) {
      if (seen[i] || Vec::is_na(cols[j][i])) continue;
      first[i] = cols[j][i];
      seen[i] = 1;
    }
  }
  bool one = nrow > 0;
  for (int i = 1; one && i < nrow; i++) {
    T left = first[i];
    T right = first[0];
    if (!same_value<RTYPE>(left, right)) one = false;
  }
  bool is_common = true;
  for (int j = 0; is_common && j < n_chunks; j++) {
    for (int i = 0; i < nrow; i++) {
      if (Vec::is_na(cols[j][i])) continue;
      T value = cols[j][i];
      T ref = first[i];
      if (one) ref = first[0];
      if (!same_value<RTYPE>(value, ref)) {
        is_common = false;
        break;
      }
    }
  }
  if (one) {
    Vec scalar(1);
    scalar[0] = first[0];
    return List::create(
      Named("common") = List::create(scalar),
      Named("is.common") = is_common);
  }
  return List::create(
    Named("common") = List::create(first),
    Named("is.common") = is_common);
}

// [[Rcpp::export]]
List common_value_for_chunks(List value_lists) {
  int n = value_lists.size();
  if (n == 0) {
    return List::create(
      Named("common") = List::create(),
      Named("is.common") = false);
  }
  int type = TYPEOF(value_lists[0]);
  for (int i = 1; i < n; i++) {
    if (TYPEOF(value_lists[i]) != type) {
      stop("common_value_for_chunks: chunk types differ");
    }
  }
  switch (type) {
  case REALSXP: return common_impl<REALSXP>(value_lists);
  case INTSXP: return common_impl<INTSXP>(value_lists);
  case LGLSXP: return common_impl<LGLSXP>(value_lists);
  case STRSXP: return common_impl<STRSXP>(value_lists);
  default: stop("common_value_for_chunks: unsupported type");
  }
}

#include <Rcpp.h>
#include <algorithm>
#include <numeric>
#include <string>
#include <unordered_map>
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

/* Same algorithm as common_value_for_group_subset_cpp, but each chunk is a
   set of absolute row indices into one column vector (no per-cell SEXPs). */
List common_value_from_row_chunks(SEXP col, const std::vector<std::vector<int> >& chunk_rows) {
  const int nchunks = static_cast<int>(chunk_rows.size());
  if (nchunks == 0) {
    return List::create(Named("common") = List(), Named("is.common") = false);
  }
  std::vector<SEXP> vecs(nchunks, col);
  std::vector<int> lvec(nchunks);
  for (int c = 0; c < nchunks; ++c) {
    lvec[c] = static_cast<int>(chunk_rows[c].size());
  }
  if (lvec[0] > 0 && std::equal(lvec.begin() + 1, lvec.end(), lvec.begin())) {
    const int gs = lvec[0];
    std::vector<CellRef> min_na(gs);
    for (int r = 0; r < gs; ++r) {
      min_na[r].chunk = 0;
      min_na[r].row = chunk_rows[0][r];
      for (int c = 0; c < nchunks; ++c) {
        const int abs_row = chunk_rows[c][r];
        if (!is_na_at(col, abs_row)) {
          min_na[r].chunk = c;
          min_na[r].row = abs_row;
          break;
        }
      }
    }
    const bool ref_scalar = all_same_refs(vecs, min_na);
    const CellRef ref0 = min_na[0];
    bool is_common = true;
    for (int c = 0; c < nchunks && is_common; ++c) {
      for (int r = 0; r < gs; ++r) {
        const int abs_row = chunk_rows[c][r];
        if (is_na_at(col, abs_row)) continue;
        CellRef ref = ref_scalar ? ref0 : min_na[r];
        if (!eq_at(col, abs_row, col, ref.row)) {
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
    for (size_t r = 0; r < chunk_rows[c].size(); ++r) {
      CellRef ref;
      ref.chunk = c;
      ref.row = chunk_rows[c][r];
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

IntegerVector column_to_codes(SEXP x) {
  const int n = Rf_length(x);
  IntegerVector out(n);
  if (TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP) {
    for (int i = 0; i < n; ++i) out[i] = INTEGER(x)[i];
    return out;
  }
  if (TYPEOF(x) == REALSXP) {
    std::unordered_map<double, int> levels;
    int next_level = 1;
    for (int i = 0; i < n; ++i) {
      double v = REAL(x)[i];
      if (ISNA(v)) {
        out[i] = NA_INTEGER;
        continue;
      }
      std::unordered_map<double, int>::iterator it = levels.find(v);
      if (it == levels.end()) {
        levels[v] = next_level;
        out[i] = next_level;
        ++next_level;
      } else {
        out[i] = it->second;
      }
    }
    return out;
  }
  CharacterVector chr = as<CharacterVector>(x);
  std::unordered_map<std::string, int> levels;
  int next_level = 1;
  for (int i = 0; i < n; ++i) {
    if (CharacterVector::is_na(chr[i])) {
      out[i] = NA_INTEGER;
      continue;
    }
    std::string key = as<std::string>(chr[i]);
    std::unordered_map<std::string, int>::iterator it = levels.find(key);
    if (it == levels.end()) {
      levels[key] = next_level;
      out[i] = next_level;
      ++next_level;
    } else {
      out[i] = it->second;
    }
  }
  return out;
}

IntegerVector make_combo_key(DataFrame built, CharacterVector vars, int n) {
  if (vars.size() == 1) {
    return column_to_codes(built[as<std::string>(vars[0])]);
  }
  std::vector<IntegerVector> codes;
  codes.reserve(vars.size());
  for (int v = 0; v < vars.size(); ++v) {
    codes.push_back(column_to_codes(built[as<std::string>(vars[v])]));
  }
  IntegerVector out(n);
  std::unordered_map<std::string, int> levels;
  int next_level = 1;
  for (int i = 0; i < n; ++i) {
    std::string key;
    bool any_na = false;
    for (size_t v = 0; v < codes.size(); ++v) {
      if (v > 0) key.push_back('\1');
      int code = codes[v][i];
      if (code == NA_INTEGER) {
        any_na = true;
        break;
      }
      key += std::to_string(code);
    }
    if (any_na) {
      out[i] = NA_INTEGER;
      continue;
    }
    std::unordered_map<std::string, int>::iterator it = levels.find(key);
    if (it == levels.end()) {
      levels[key] = next_level;
      out[i] = next_level;
      ++next_level;
    } else {
      out[i] = it->second;
    }
  }
  return out;
}

struct ChunkRows {
  int first_row;
  std::vector<int> rows;
};

struct GroupBuckets {
  int first_row;
  std::unordered_map<int, ChunkRows> chunks;
};

SEXP build_group_column(SEXP group_col, const std::vector<int>& first_rows) {
  const int n = static_cast<int>(first_rows.size());
  switch (TYPEOF(group_col)) {
  case REALSXP: {
    NumericVector out(n);
    for (int i = 0; i < n; ++i) out[i] = REAL(group_col)[first_rows[i]];
    return out;
  }
  case INTSXP: {
    IntegerVector out(n);
    for (int i = 0; i < n; ++i) out[i] = INTEGER(group_col)[first_rows[i]];
    return out;
  }
  case LGLSXP: {
    LogicalVector out(n);
    for (int i = 0; i < n; ++i) out[i] = LOGICAL(group_col)[first_rows[i]];
    return out;
  }
  case STRSXP: {
    CharacterVector out(n);
    for (int i = 0; i < n; ++i) out[i] = STRING_ELT(group_col, first_rows[i]);
    return out;
  }
  default: {
    List out(n);
    for (int i = 0; i < n; ++i) out[i] = R_NilValue;
    return out;
  }
  }
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

/* Outer column/group scan for issue #258: avoid data.table by= allocations
   by bucketing row indices once, then comparing inside each (col, group). */
// [[Rcpp::export]]
List detect_common_value_dt_cpp(DataFrame built,
                                CharacterVector col_name_vec,
                                CharacterVector chunk_vars) {
  const int n = built.nrows();
  const int ncol = col_name_vec.size();
  if (ncol == 0 || n == 0) {
    return List::create(
      Named("col.name") = CharacterVector(),
      Named("group") = IntegerVector(),
      Named("common") = List(),
      Named("is.common") = LogicalVector()
    );
  }

  SEXP group_col = built["group"];
  IntegerVector group_key = column_to_codes(group_col);
  IntegerVector chunk_key = make_combo_key(built, chunk_vars, n);

  /* group_key -> buckets of chunk -> row indices (column-independent). */
  std::unordered_map<int, GroupBuckets> groups;
  std::vector<int> group_order;
  group_order.reserve(static_cast<size_t>(n));
  for (int i = 0; i < n; ++i) {
    const int gk = group_key[i];
    const int ck = chunk_key[i];
    std::unordered_map<int, GroupBuckets>::iterator git = groups.find(gk);
    if (git == groups.end()) {
      GroupBuckets gb;
      gb.first_row = i;
      ChunkRows cr;
      cr.first_row = i;
      cr.rows.push_back(i);
      gb.chunks[ck] = cr;
      groups[gk] = gb;
      group_order.push_back(gk);
    } else {
      std::unordered_map<int, ChunkRows>::iterator cit = git->second.chunks.find(ck);
      if (cit == git->second.chunks.end()) {
        ChunkRows cr;
        cr.first_row = i;
        cr.rows.push_back(i);
        git->second.chunks[ck] = cr;
      } else {
        cit->second.rows.push_back(i);
      }
    }
  }

  const int ngroup = static_cast<int>(group_order.size());
  const int nout = ncol * ngroup;

  CharacterVector out_col(nout);
  LogicalVector out_is_common(nout);
  List out_common(nout);
  std::vector<int> group_first_rows(static_cast<size_t>(nout));

  /* Precompute ordered chunk row vectors per group (same for every column). */
  std::vector<std::vector<std::vector<int> > > group_chunks(static_cast<size_t>(ngroup));
  for (int g = 0; g < ngroup; ++g) {
    GroupBuckets& gb = groups[group_order[g]];
    std::vector<int> chunk_ids;
    chunk_ids.reserve(gb.chunks.size());
    for (std::unordered_map<int, ChunkRows>::iterator cit = gb.chunks.begin();
         cit != gb.chunks.end(); ++cit) {
      chunk_ids.push_back(cit->first);
    }
    std::sort(chunk_ids.begin(), chunk_ids.end(), [&](int a, int b) {
      return gb.chunks.find(a)->second.first_row < gb.chunks.find(b)->second.first_row;
    });
    group_chunks[g].reserve(chunk_ids.size());
    for (size_t c = 0; c < chunk_ids.size(); ++c) {
      group_chunks[g].push_back(gb.chunks.find(chunk_ids[c])->second.rows);
    }
  }

  int out_i = 0;
  for (int j = 0; j < ncol; ++j) {
    const std::string col_name = as<std::string>(col_name_vec[j]);
    SEXP col = built[col_name];
    for (int g = 0; g < ngroup; ++g, ++out_i) {
      out_col[out_i] = col_name;
      group_first_rows[out_i] = groups[group_order[g]].first_row;
      List one = common_value_from_row_chunks(col, group_chunks[g]);
      out_is_common[out_i] = as<bool>(one["is.common"]);
      /* clone so the nested common SEXP stays protected after `one` dies */
      out_common[out_i] = clone(as<RObject>(one["common"]));
    }
  }

  return List::create(
    Named("col.name") = out_col,
    Named("group") = build_group_column(group_col, group_first_rows),
    Named("common") = out_common,
    Named("is.common") = out_is_common
  );
}

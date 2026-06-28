#include <Rcpp.h>
#include <algorithm>
#include <string>
#include <unordered_map>
#include <vector>

using namespace Rcpp;

namespace {

inline bool is_na_sexp(SEXP x) {
  switch (TYPEOF(x)) {
  case REALSXP:
    return ISNA(REAL(x)[0]);
  case INTSXP:
    return INTEGER(x)[0] == NA_INTEGER;
  case LGLSXP:
    return LOGICAL(x)[0] == NA_LOGICAL;
  case STRSXP:
    return STRING_ELT(x, 0) == NA_STRING;
  default:
    return false;
  }
}

inline bool equal_sexp(SEXP a, SEXP b) {
  if (a == R_NilValue && b == R_NilValue) return true;
  if (a == R_NilValue || b == R_NilValue) return false;
  if (TYPEOF(a) != TYPEOF(b)) return false;
  switch (TYPEOF(a)) {
  case REALSXP: {
    double da = REAL(a)[0], db = REAL(b)[0];
    if (ISNA(da) && ISNA(db)) return true;
    return da == db;
  }
  case INTSXP: {
    int da = INTEGER(a)[0], db = INTEGER(b)[0];
    if (da == NA_INTEGER && db == NA_INTEGER) return true;
    return da == db;
  }
  case LGLSXP: {
    int da = LOGICAL(a)[0], db = LOGICAL(b)[0];
    if (da == NA_LOGICAL && db == NA_LOGICAL) return true;
    return da == db;
  }
  case STRSXP:
    return STRING_ELT(a, 0) == STRING_ELT(b, 0);
  default:
    return false;
  }
}

SEXP get_elem(SEXP col, int i) {
  switch (TYPEOF(col)) {
  case REALSXP:
    return Rf_ScalarReal(REAL(col)[i]);
  case INTSXP:
    return Rf_ScalarInteger(INTEGER(col)[i]);
  case LGLSXP:
    return Rf_ScalarLogical(LOGICAL(col)[i]);
  case STRSXP:
    return Rf_ScalarString(STRING_ELT(col, i));
  default:
    return R_NilValue;
  }
}

bool all_equal_na_rm_matrix(const std::vector<SEXP>& values,
                            const std::vector<SEXP>& ref,
                            int group_size) {
  const bool ref_scalar = ref.size() == 1;
  for (size_t i = 0; i < values.size(); ++i) {
    if (is_na_sexp(values[i])) continue;
    const int row = static_cast<int>(i % group_size);
    const SEXP r = ref_scalar ? ref[0] : ref[row];
    if (!equal_sexp(values[i], r)) return false;
  }
  return true;
}

SEXP first_non_na(const std::vector<SEXP>& row_values) {
  for (size_t i = 0; i < row_values.size(); ++i) {
    if (!is_na_sexp(row_values[i])) return row_values[i];
  }
  return row_values.empty() ? R_NilValue : row_values[0];
}

bool all_refs_equal(const std::vector<SEXP>& refs) {
  if (refs.empty()) return true;
  for (size_t i = 1; i < refs.size(); ++i) {
    if (!equal_sexp(refs[0], refs[i])) return false;
  }
  return true;
}

bool all_values_unique_one(const std::vector<SEXP>& values) {
  if (values.empty()) return false;
  for (size_t i = 1; i < values.size(); ++i) {
    if (!equal_sexp(values[0], values[i])) return false;
  }
  return true;
}

List common_value_for_group_subset_cpp(const std::vector<std::vector<SEXP> >& chunk_values) {
  std::vector<int> lvec(chunk_values.size());
  std::vector<SEXP> value_vec;
  for (size_t c = 0; c < chunk_values.size(); ++c) {
    lvec[c] = static_cast<int>(chunk_values[c].size());
    value_vec.insert(value_vec.end(), chunk_values[c].begin(), chunk_values[c].end());
  }
  if (lvec.size() > 0 && std::equal(lvec.begin() + 1, lvec.end(), lvec.begin())) {
    const int group_size = lvec[0];
    const int nchunks = static_cast<int>(lvec.size());
    std::vector<SEXP> min_na_vec;
    min_na_vec.reserve(group_size);
    for (int row = 0; row < group_size; ++row) {
      std::vector<SEXP> row_values;
      row_values.reserve(nchunks);
      for (int col = 0; col < nchunks; ++col) {
        row_values.push_back(value_vec[row + col * group_size]);
      }
      min_na_vec.push_back(first_non_na(row_values));
    }
    std::vector<SEXP> ref = min_na_vec;
    if (all_refs_equal(min_na_vec)) {
      ref.assign(1, min_na_vec[0]);
    }
    const bool is_common = all_equal_na_rm_matrix(value_vec, ref, group_size);
    if (ref.size() == 1) {
      return List::create(
        Named("common") = List::create(ref[0]),
        Named("is.common") = is_common
      );
    }
    return List::create(
      Named("common") = List(min_na_vec.begin(), min_na_vec.end()),
      Named("is.common") = is_common
    );
  }
  if (all_values_unique_one(value_vec)) {
    return List::create(
      Named("common") = List::create(value_vec[0]),
      Named("is.common") = true
    );
  }
  return List::create(
    Named("common") = List(),
    Named("is.common") = false
  );
}

IntegerVector factor_to_int(SEXP x) {
  if (Rf_isFactor(x)) {
    return IntegerVector(x);
  }
  if (TYPEOF(x) == INTSXP) {
    return IntegerVector(x);
  }
  CharacterVector chr = as<CharacterVector>(x);
  IntegerVector out(chr.size());
  std::unordered_map<std::string, int> levels;
  int next_level = 1;
  for (int i = 0; i < chr.size(); ++i) {
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

struct PairKey {
  int col;
  int group_key;
  bool operator==(const PairKey& other) const {
    return col == other.col && group_key == other.group_key;
  }
};

struct PairKeyHash {
  std::size_t operator()(const PairKey& k) const {
    return static_cast<std::size_t>(k.col) * 1000003u + static_cast<std::size_t>(k.group_key);
  }
};

struct ChunkData {
  std::vector<SEXP> values;
  int first_row;
};

SEXP build_group_output(const std::vector<SEXP>& groups, SEXP template_group) {
  const int n = static_cast<int>(groups.size());
  switch (TYPEOF(template_group)) {
  case REALSXP: {
    NumericVector out(n);
    for (int i = 0; i < n; ++i) out[i] = REAL(groups[i])[0];
    return out;
  }
  case INTSXP: {
    IntegerVector out(n);
    for (int i = 0; i < n; ++i) out[i] = INTEGER(groups[i])[0];
    return out;
  }
  case LGLSXP: {
    LogicalVector out(n);
    for (int i = 0; i < n; ++i) out[i] = LOGICAL(groups[i])[0];
    return out;
  }
  case STRSXP: {
    CharacterVector out(n);
    for (int i = 0; i < n; ++i) out[i] = STRING_ELT(groups[i], 0);
    return out;
  }
  default:
    return List(groups.begin(), groups.end());
  }
}

}  // namespace

// [[Rcpp::export]]
List detect_common_value_dt_cpp(DataFrame built,
                                CharacterVector col_name_vec,
                                CharacterVector chunk_vars) {
  const int n = built.nrows();
  SEXP group_col = built["group"];
  IntegerVector group_key = factor_to_int(group_col);

  IntegerVector chunk_key(n);
  if (chunk_vars.size() == 1) {
    chunk_key = factor_to_int(built[as<std::string>(chunk_vars[0])]);
  } else {
    CharacterVector combo(n);
    for (int i = 0; i < n; ++i) {
      std::string key;
      for (int v = 0; v < chunk_vars.size(); ++v) {
        if (v > 0) key += "\1";
        SEXP col = built[as<std::string>(chunk_vars[v])];
        key += as<std::string>(as<CharacterVector>(col)[i]);
      }
      combo[i] = key;
    }
    chunk_key = factor_to_int(combo);
  }

  typedef std::unordered_map<int, ChunkData> ChunkMap;
  std::unordered_map<PairKey, ChunkMap, PairKeyHash> buckets;
  std::unordered_map<PairKey, SEXP, PairKeyHash> original_group;

  for (int j = 0; j < col_name_vec.size(); ++j) {
    const std::string col_name = as<std::string>(col_name_vec[j]);
    SEXP col = built[col_name];
    for (int i = 0; i < n; ++i) {
      PairKey key;
      key.col = j + 1;
      key.group_key = group_key[i];
      if (original_group.find(key) == original_group.end()) {
        original_group[key] = get_elem(group_col, i);
      }
      ChunkData& chunk = buckets[key][chunk_key[i]];
      if (chunk.values.empty()) {
        chunk.first_row = i;
      }
      chunk.values.push_back(get_elem(col, i));
    }
  }

  std::vector<std::string> out_col;
  std::vector<SEXP> out_group;
  std::vector<bool> out_is_common;
  List out_common;

  out_col.reserve(buckets.size());
  out_group.reserve(buckets.size());
  out_is_common.reserve(buckets.size());
  out_common = List(static_cast<int>(buckets.size()));

  int out_i = 0;
  for (std::unordered_map<PairKey, ChunkMap, PairKeyHash>::iterator it = buckets.begin();
       it != buckets.end(); ++it, ++out_i) {
    out_col.push_back(as<std::string>(col_name_vec[it->first.col - 1]));
    out_group.push_back(original_group[it->first]);

    std::vector<int> chunk_ids;
    chunk_ids.reserve(it->second.size());
    for (ChunkMap::iterator cit = it->second.begin(); cit != it->second.end(); ++cit) {
      chunk_ids.push_back(cit->first);
    }
    std::sort(chunk_ids.begin(), chunk_ids.end(), [&](int a, int b) {
      return it->second[a].first_row < it->second[b].first_row;
    });

    std::vector<std::vector<SEXP> > chunk_values;
    chunk_values.reserve(chunk_ids.size());
    for (size_t c = 0; c < chunk_ids.size(); ++c) {
      chunk_values.push_back(it->second[chunk_ids[c]].values);
    }

    List one = common_value_for_group_subset_cpp(chunk_values);
    out_is_common.push_back(as<bool>(one["is.common"]));
    out_common[out_i] = one["common"];
  }

  return List::create(
    Named("col.name") = CharacterVector(out_col.begin(), out_col.end()),
    Named("group") = build_group_output(out_group, group_col),
    Named("common") = out_common,
    Named("is.common") = LogicalVector(out_is_common.begin(), out_is_common.end())
  );
}

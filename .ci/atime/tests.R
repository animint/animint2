animint2_atime_pkg_edit <- function(old.Package, new.Package, sha, new.pkg.path) {
  get("pkg.edit.default", envir=asNamespace("atime"))(
    old.Package=old.Package,
    new.Package=new.Package,
    sha=sha,
    new.pkg.path=new.pkg.path)
  rd_replacement <- sprintf(
    "get(\"rd_aesthetics\", envir=asNamespace(\"%s\"))",
    new.Package)
  rd_files <- Sys.glob(file.path(new.pkg.path, "man", "*.Rd"))
  for (rd_file in rd_files) {
    rd_lines <- readLines(rd_file)
    new_lines <- gsub(
      "animint2:::rd_aesthetics",
      rd_replacement,
      rd_lines,
      fixed=TRUE)
    if (!identical(rd_lines, new_lines)) writeLines(new_lines, rd_file)
  }
}

test.list <- atime::atime_test_list(
  ## Adapted from https://github.com/animint/animint2/issues/235#issuecomment-3342083861
  "getCommonChunk improved in #238"=atime::atime_test(
    expr=animint2:::getCommonChunk(built, "showSelected", list(group="group")),
    setup={
      built <- data.table(
        x=1:N,
        group=rep(seq(1,N/2), each=2),
        showSelected=1:2)
    },
    seconds.limit=1,
    pkg.edit.fun=animint2_atime_pkg_edit,
    Slow="352f7e10040cb9de6ddd16416d342e9746c14c7a", # Parent of the first commit (https://github.com/animint/animint2/commit/121a11399e7d6ca6c822cd22472886c6d4d8cf10) of the PR (https://github.com/animint/animint2/pull/238/commits).
    Fast="30950779702e6c8aeecd24aeb737c9fa5ce898e0") # Last commit in the PR (https://github.com/animint/animint2/pull/238/commits).
)

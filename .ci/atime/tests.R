test.list <- atime::atime_test_list(
  pkg.edit.fun = function(old.Package, new.Package, sha, new.pkg.path) {
    unlink(file.path(new.pkg.path, "man", "*")) # to avoid broken Sexpr https://github.com/animint/animint2/issues/340
  },
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
    Slow="352f7e10040cb9de6ddd16416d342e9746c14c7a", # Parent of the first commit (https://github.com/animint/animint2/commit/121a11399e7d6ca6c822cd22472886c6d4d8cf10) of the PR (https://github.com/animint/animint2/pull/238/commits).
    Fast="fe051f44b61cc9f494387165ff894c14fd4e077c") # Merge commit of the PR (https://github.com/animint/animint2/pull/238).
)

test.list <- atime::atime_test_list(
  pkg.edit.fun = function(old.Package, new.Package, sha, new.pkg.path) {
    atime:::pkg.edit.default(old.Package, new.Package, sha, new.pkg.path)
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
    Fast="fe051f44b61cc9f494387165ff894c14fd4e077c", # Merge commit of the first speed improvement PR (https://github.com/animint/animint2/pull/238) in Sep 2026.
    bisect.commits=c(
      CIfixed="1295c2e4381dedcad8c10d25e7120342666fbd0a") # Merge commit of PR which fixed atime CI in Nov 2026.
  )
)

test.list <- atime::atime_test_list(
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
    Fast="30950779702e6c8aeecd24aeb737c9fa5ce898e0") # Last commit in the PR (https://github.com/animint/animint2/pull/238/commits).
)

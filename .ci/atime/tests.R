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
    Slow="352f7e10040cb9de6ddd16416d342e9746c14c7a", # Parent of the first commit (https://github.com/animint/animint2/commit/121a11399e7d6ca6c822cd22472886c6d4d8cf10) of the PR (https://github.com/animint/animint2/pull/238/commits).
    Fast="30950779702e6c8aeecd24aeb737c9fa5ce898e0"), # Last commit in the PR (https://github.com/animint/animint2/pull/238/commits).
  ## Post-#242 workload with NA groups and multiple common columns (#258).
  ## Slow = master before C++ (parent of first C++ commit on PR #342).
  ## Fast = tip of PR #342 with common_value_for_group_subset_cpp().
  "getCommonChunk C++ #258"=atime::atime_test(
    expr=animint2:::getCommonChunk(built, "showSelected", list(group="group")),
    setup={
      built <- data.table(
        x=rep(1:5, length.out=N),
        y=rnorm(N),
        colour="foo",
        group=rep(seq_len(N/4), each=4),
        showSelected=rep(1:2, each=N/2),
        na_group=rep(c(0,0,1,0), length.out=N),
        row_in_group=rep(1:4, length.out=N)
      )
    },
    seconds.limit=2,
    Slow="9dce8611495357d4441793b9494bbce11fcc1a9f", # Parent of first C++ commit https://github.com/animint/animint2/commit/623545cb
    Fast="5cc3f80a33755cbaa4cb10c21bf868947b112178") # CRAN-NOTES fix on PR https://github.com/animint/animint2/pull/342
)

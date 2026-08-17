test.list <- atime::atime_test_list(
  ## Historical #238 Slow/Fast commits fail `R CMD INSTALL` on current CI:
  ## old man/geom_dotplot.Rd still has build-stage \Sexpr that looks up
  ## GeomDotplot, which errors with "No geom called GeomDotplot".
  ## Keep only the #258 C++ comparison for PR
  ## https://github.com/animint/animint2/pull/342
  ##
  ## Issue #258: column/group C++ scan vs pre-C++ master.
  ## Slow = before first C++ commit on PR #342 (R by= loop).
  ## Fast = inner-compare-only C++ (188cb342). After this commit, HEAD is
  ## the full detect_common_value_dt_cpp scan; CI also times HEAD.
  ## Workload must (1) put each group in both showSelected values and
  ## (2) produce a non-NULL common chunk, or the bench never hits C++.
  "getCommonChunk C++ #258"=atime::atime_test(
    expr=animint2:::getCommonChunk(built, "showSelected", list(group="group")),
    setup={
      ## atime supplies N; keep row count divisible by 4 for this workload.
      n <- 4L * as.integer(N / 4L)
      if(n < 4L) n <- 4L
      ng <- as.integer(n / 4L)
      built <- data.table(
        x=rep(seq_len(ng), each=4L),
        y=rep(seq_len(ng), each=4L),
        fill=rep(c("a","a","b","b"), length.out=n),
        group=rep(seq_len(ng), each=4L),
        showSelected=rep(c(1L, 1L, 2L, 2L), length.out=n),
        na_group=0L,
        row_in_group=rep(1:4, length.out=n)
      )
    },
    seconds.limit=2,
    Slow="9dce8611495357d4441793b9494bbce11fcc1a9f", # Parent of first C++ commit https://github.com/animint/animint2/commit/623545cb
    Fast="188cb3422ee0c87d9a71c06ab42a67b9dccb4be0") # Inner-compare-only C++; HEAD after grouping-scan commit is the real Fast path
)

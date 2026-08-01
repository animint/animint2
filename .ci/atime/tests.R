test.list <- atime::atime_test_list(
  ## Historical #238 Slow/Fast commits fail `R CMD INSTALL` on current CI:
  ## old man/geom_dotplot.Rd still has build-stage \Sexpr that looks up
  ## GeomDotplot, which errors with "No geom called GeomDotplot".
  ## Keep only the #258 C++ comparison for PR
  ## https://github.com/animint/animint2/pull/342
  ##
  ## Issue #258: C++ inner compare vs pre-C++ master.
  ## Slow = before first C++ commit on PR #342.
  ## Fast = C++ + GC-safe rewrite (common_value_for_group_subset_cpp).
  "getCommonChunk C++ #258"=atime::atime_test(
    expr=animint2:::getCommonChunk(built, "showSelected", list(group="group")),
    setup={
      ## atime supplies N; keep row count divisible by 4 for this workload.
      n <- 4L * as.integer(N / 4L)
      if(n < 4L) n <- 4L
      built <- data.table(
        x=rep(1:5, length.out=n),
        y=rnorm(n),
        colour="foo",
        group=rep(seq_len(n/4L), each=4L),
        showSelected=rep(1:2, each=n/2L),
        na_group=rep(c(0,0,1,0), length.out=n),
        row_in_group=rep(1:4, length.out=n)
      )
    },
    seconds.limit=2,
    Slow="9dce8611495357d4441793b9494bbce11fcc1a9f", # Parent of first C++ commit https://github.com/animint/animint2/commit/623545cb
    Fast="188cb3422ee0c87d9a71c06ab42a67b9dccb4be0") # Review fixes with GC-safe C++ on PR https://github.com/animint/animint2/pull/342
)

test.list <- atime::atime_test_list(
  ## Adapted from https://github.com/animint/animint2/issues/235#issuecomment-3342083861
  ## The #238 SHAs do not install: geom_dotplot.Rd looks up GeomDotplot,
  ## which was removed in #311. Slow is master before the C++ compare.
  ## HEAD is this branch. Fast stays unset until a faster commit is on master.
  "getCommonChunk C++ compare #258"=atime::atime_test(
    expr=animint2:::getCommonChunk(built, "showSelected", list(group="group")),
    setup={
      built <- data.table(
        x=1:N,
        group=rep(seq(1,N/2), each=2),
        showSelected=1:2)
    },
    seconds.limit=1,
    Slow="37b138e684f40b8c765bb1dd8fcf9563f23a2e03")
)

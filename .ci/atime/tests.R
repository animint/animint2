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
    Slow="a4220dfe02214969964f97d69f1516d75b9d961a", # master before coord_equal fix; installable on R 4.6 (352f7e fails Rd build in atime CI).
    Fast="109e061a6e6d5f6d9eee9406697308b890e236f7") # test-coord_equal branch head (PR #253).
)

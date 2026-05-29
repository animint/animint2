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
    Slow="e32921ce2d884f3543658997fd8c157036ba49ca", # Before #238 getCommonChunk refactor (split/loop); installable on R 4.6. Intended parent 352f7e fails atime CI (geom_dotplot Rd build).
    Fast="30950779702e6c8aeecd24aeb737c9fa5ce898e0") # Last commit of PR #238 (data.table getCommonChunk + na.rm=TRUE).
)

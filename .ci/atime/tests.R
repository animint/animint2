# Performance tests for the atime GitHub Action (see .github/workflows/atime.yaml).
# Results are posted as a PR comment when R/ or .ci/atime/ changes.
#
# Test "getCommonChunk improved in #238" (NEWS 2025.9.27, PR #238) times getCommonChunk.
# Commits before #311 ship man/geom_dotplot.Rd with rd_aesthetics() Sexpr; that breaks
# R CMD INSTALL on R 4.6 in CI. pkg.edit.fun stubs that Rd for old checkouts only.

atime_stub_geom_dotplot_rd <- function(pkg.path) {
  rd <- file.path(pkg.path, "man", "geom_dotplot.Rd")
  if (file.exists(rd)) {
    lines <- readLines(rd, warn = FALSE)
    if (any(grepl("rd_aesthetics", lines, fixed = TRUE))) {
      writeLines(c(
        "% Please edit documentation in R/geom-dotplot.r",
        "\\name{geom_dotplot}",
        "\\alias{geom_dotplot}",
        "\\title{Dot plot}",
        "\\description{Stubbed for atime install of a pre-#311 commit.}",
        "\\keyword{internal}"
      ), rd)
    }
  }
  invisible()
}

test.list <- atime::atime_test_list(
  pkg.edit.fun=atime_stub_geom_dotplot_rd,
  "getCommonChunk improved in #238"=atime::atime_test(
    expr=animint2:::getCommonChunk(built, "showSelected", list(group="group")),
    setup={
      built <- data.table(
        x=1:N,
        group=rep(seq(1,N/2), each=2),
        showSelected=1:2)
    },
    seconds.limit=1,
    Slow="352f7e10040cb9de6ddd16416d342e9746c14c7a",
    Fast="30950779702e6c8aeecd24aeb737c9fa5ce898e0")
)

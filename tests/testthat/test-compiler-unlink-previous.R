acontext("compiler unlink previous")

data(WorldBank)

out.dir <- file.path(tempdir(), "animint2-wb")
unlink(out.dir, recursive=TRUE)
viz <- list(
  scatter=ggplot()+
    geom_point(aes(
      fertility.rate, life.expectancy, color=region),
      showSelected="year",
      chunk_vars=character(),
      data=WorldBank))
animint2dir(viz, out.dir, open.browser=FALSE)

viz2 <- list(
  scatter=ggplot()+
    geom_text(aes(
      fertility.rate, life.expectancy, label=country),
      data=WorldBank,
      showSelected=c("year", "country"),
      chunk_vars=character())+
    geom_point(aes(
      fertility.rate, life.expectancy, color=region),
      showSelected="year",
      chunk_vars=character(),
      data=WorldBank))
animint2dir(viz2, out.dir, open.browser=FALSE)

test_that("only two geom.tsv files in out.dir", {
  geom.tsv.vec <- grep("^geom", dir(out.dir), value=TRUE)
  expected.tsv.vec <- c(
    "geom1_text_scatter_chunk1.tsv",
    "geom2_point_scatter_chunk1.tsv")
  expect_identical(geom.tsv.vec, expected.tsv.vec)
})

test_that("error if dir exists and no animint.js inside", {
  animint.js <- file.path(out.dir, "animint.js")
  unlink(animint.js)
  expect_error({
    animint2dir(viz2, out.dir, open.browser=FALSE)
  }, "animint.js does not exist, so not removing out.dir. If you really want to save your animint in out.dir, then please remove that directory entirely")
})


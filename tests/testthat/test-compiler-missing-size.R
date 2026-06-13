line_dt <- data.frame(
  x=1,
  y=2,
  other="foo",
  algo="miss")
viz <- animint(
  ggplot()+
    geom_line(aes(
      x, y, color=algo, size=algo),
      showSelected="other",
      data=line_dt)+
    scale_size_manual(values=c(missing=2)))
test_that("informative error for missing data", {
  expect_error({
    animint2dir(viz)
  }, "geom has 1 row(s) all of which are missing, please make sure manual scale values match data values", fixed=TRUE)
})

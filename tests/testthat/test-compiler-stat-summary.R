acontext("stat_summary")

test_that("stat_summary does not infinitely recurse", {
  data(UStornadoes, package = "animint2")
  gg <- ggplot()+
    stat_summary(aes(year, year),
                 data=UStornadoes,
                 clickSelects="year", fun.y=length, geom="bar")
  L <- list(bar=gg)
  info <- animint2dir(L, open.browser=FALSE)
  expect_identical(length(info$geoms), 1L)
})

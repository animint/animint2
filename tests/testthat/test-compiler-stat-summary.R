acontext("a_stat_summary")

test_that("a_stat_summary does not infinitely recurse", {
  data(UStornadoes, package = "animint2")
  gg <- a_plot()+
    a_stat_summary(aes(year, year),
                 data=UStornadoes,
                 clickSelects="year", fun.y=length, geom="bar")
  L <- list(bar=gg)
  info <- animint2dir(L, open.browser=FALSE)
  expect_identical(length(info$geoms), 1L)
})

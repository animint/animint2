acontext("a_stat_summary")

test_that("a_stat_summary does not infinitely recurse", {
  data(UStornadoes, package = "animint2")
  gg <- a_plot()+
    a_stat_summary(a_aes(year, year),
                 data=UStornadoes,
                 clickSelects="year", fun.y=length, a_geom="bar")
  L <- list(bar=gg)
  info <- animint2dir(L, open.browser=FALSE)
  expect_identical(length(info$geoms), 1L)
})

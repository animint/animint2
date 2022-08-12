acontext("compiler errors")

library(animint2)
viz <- list(
  gg=ggplot()+
    geom_tallrect(aes(
      xmin=day.of.the.month-0.5, xmax=day.of.the.month+0.5,
      key=paste(day.POSIXct)),
      showSelected="month",
      color="grey",
      data=data.frame(
        day.of.the.month=integer(),
        day.POSIXct=character(),
        month=character())))

test_that("informative error for empty data table", {
  expect_error({
    animint2dir(viz, open.browser=FALSE)
  }, "no data in geom1_tallrect_gg")
})

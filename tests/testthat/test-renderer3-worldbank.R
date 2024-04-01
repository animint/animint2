acontext("TestWorldBank")

data(WorldBank, package="animint2")
WorldBank$Region <- sub(" (all income levels)", "", WorldBank$region, fixed=TRUE)
WorldBank1975 <- subset(WorldBank, year==1975)

scatter <- ggplot()+
  geom_point(
    mapping=aes(x=life.expectancy, y=fertility.rate, color=Region),
    data=WorldBank1975)

animint(scatter)
scatter_plot <- list(plot = scatter)

test_that("worldbank scatter generated without any warnings or errors", {
  expect_no_warning({
    info <- animint2HTML(scatter_plot)
  })
}) 
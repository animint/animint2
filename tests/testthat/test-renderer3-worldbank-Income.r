acontext("TestWorldBank")

data(WorldBank, package="animint2")
WorldBank1975 <- subset(WorldBank, year==1975)
income.colors <- 
  c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33")
WorldBank$income = factor(WorldBank$income,levels = c("High income: nonOECD","High income: OECD","Upper middle income","Lower middle income","Low income","Not classified"))
names(income.colors) <- levels(WorldBank$income)
scatter <- ggplot()+
  scale_color_manual(values=income.colors)+
  geom_point(
    mapping=aes(x=life.expectancy, y=fertility.rate, color=income),
    data=WorldBank1975)


animint(scatter)
scatter_plot <- list(plot = scatter)

test_that("worldbank scatter generated without any warnings or errors", {
  expect_no_warning({
    info <- animint2HTML(scatter_plot)
  })
})
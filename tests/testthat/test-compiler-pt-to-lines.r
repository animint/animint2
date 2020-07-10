acontext("pt/points to lines conversion test")

viz <- list(
  p1 = ggplot()+
    geom_point(aes(Petal.Length, Sepal.Length, color=Species),data=iris))

theme.pars <- plot_theme(viz)
panel_margin_lines <- pt.to.lines(theme.pars$panel.margin)

if(grid::unitType(theme.pars$panel.margin) == "pt" || grid::unitType(theme.pars$panel.margin) == "points") {
  test_that("after conversion values unequal", {
    expect_false(identical(panel_margin_lines, theme.pars$panel.margin))
  })
}
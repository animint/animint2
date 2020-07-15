acontext("pt/points to lines conversion test")

viz <- list(
  p1 = ggplot()+
    geom_point(aes(Petal.Length, Sepal.Length, color=Species),data=iris))

theme.pars <- plot_theme(viz)
panel_margin_lines <- pt.to.lines(theme.pars$panel.margin)

if(grid::unitType(theme.pars$panel.margin) %in% c("pt", "points")) {
  test_that("after conversion values unequal", {
    # The values shouldn't be identical because we're applying operations over the value.
    # new_val <- round(as.numeric(pt_value) * (0.25/5.5), digits = 2)
    # pt.to.lines() should return different value if unit passed is pt/points.
    expect_false(identical(panel_margin_lines, theme.pars$panel.margin))
  })
}
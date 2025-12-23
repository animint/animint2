acontext("panel.margin with positive values - Issue #180")
test_that("pt.to.lines handles positive lines unit correctly", {
  lines_value <- grid::unit(2, "lines")
  converted <- pt.to.lines(lines_value)
  expect_true(is.numeric(converted))
  expect_equal(converted, 2)
  expect_gt(converted, 0)
})
test_that("pt.to.lines handles positive cm unit correctly", {
  cm_value <- grid::unit(0.5, "cm")
  converted <- pt.to.lines(cm_value)
  expect_true(is.numeric(converted))
  expect_gt(converted, 0)
  expect_equal(as.numeric(cm_value), converted)
})
test_that("pt.to.lines handles positive pt unit correctly", {
  pt_value <- grid::unit(12, "pt")
  converted <- pt.to.lines(pt_value)
  expect_true(is.numeric(converted))
  expect_gt(converted, 0)
  expect_false(identical(converted, as.numeric(pt_value)))
  expected <- round(as.numeric(pt_value) * (0.25/5.5), digits = 2)
  expect_equal(converted, expected)
})
viz.default <- list(
  p1 = ggplot() +
    geom_point(aes(Petal.Length, Sepal.Length, color = Species), data = iris))
test_that("plot_theme extracts panel.margin correctly for default theme", {
  theme.pars <- plot_theme(viz.default$p1)
  panel_margin <- theme.pars$panel.margin
  expect_false(is.null(panel_margin))
  expect_true(grid::is.unit(panel_margin))
})
test_that("positive lines preserved through plot_theme and pt.to.lines", {
  viz <- list(
    p1 = ggplot() +
      geom_point(aes(Petal.Length, Sepal.Length, color = Species), data = iris) +
      theme(panel.margin = grid::unit(2, "lines")))
  theme.pars <- plot_theme(viz$p1)
  panel_margin <- theme.pars$panel.margin
  expect_false(is.null(panel_margin))
  converted <- pt.to.lines(panel_margin)
  expect_true(is.numeric(converted))
  expect_equal(converted, 2)
})
test_that("positive cm preserved through plot_theme and pt.to.lines", {
  viz <- list(
    p1 = ggplot() +
      geom_point(aes(Petal.Length, Sepal.Length, color = Species), data = iris) +
      theme(panel.margin = grid::unit(1, "cm")))
  theme.pars <- plot_theme(viz$p1)
  panel_margin <- theme.pars$panel.margin
  expect_false(is.null(panel_margin))
  converted <- pt.to.lines(panel_margin)
  expect_true(is.numeric(converted))
  expect_equal(converted, 1)
})
test_that("zero panel.margin should result in zero spacing", {
  viz <- list(
    p1 = ggplot() +
      geom_point(aes(Petal.Length, Sepal.Length, color = Species), data = iris) +
      theme(panel.margin = grid::unit(0, "lines")))
  theme.pars <- plot_theme(viz$p1)
  panel_margin <- theme.pars$panel.margin
  converted <- pt.to.lines(panel_margin)
  expect_equal(converted, 0)
})
test_that("positive panel.margin in lines greater than zero", {
  viz.positive <- list(
    p1 = ggplot() +
      geom_point(aes(Petal.Length, Sepal.Length, color = Species), data = iris) +
      theme(panel.margin = grid::unit(2, "lines")))
  viz.zero <- list(
    p1 = ggplot() +
      geom_point(aes(Petal.Length, Sepal.Length, color = Species), data = iris) +
      theme(panel.margin = grid::unit(0, "lines")))
  theme.positive <- plot_theme(viz.positive$p1)
  theme.zero <- plot_theme(viz.zero$p1)
  converted.positive <- pt.to.lines(theme.positive$panel.margin)
  converted.zero <- pt.to.lines(theme.zero$panel.margin)
  expect_equal(converted.zero, 0)
  expect_gt(converted.positive, converted.zero)
  expect_equal(converted.positive, 2)
})

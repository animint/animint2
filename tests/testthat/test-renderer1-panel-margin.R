acontext("panel.margin positive values - Issue #180 - renderer")
library(XML)
panel_attr <- function(node_list, name) {
  as.numeric(sapply(node_list, xmlAttrs)[name, ])
}
gap_between_positions <- function(positions, sizes) {
  u <- sort(unique(positions))
  if (length(u) < 2) return(NA_real_)
  tol <- 1e-4
  i1 <- abs(positions - u[1]) < tol
  i2 <- abs(positions - u[2]) < tol
  min(positions[i2]) - max(positions[i1] + sizes[i1])
}

viz.zero <- list(
  plot1 = ggplot() +
    theme_bw() +
    theme(panel.margin = grid::unit(0, "lines")) +
    geom_point(aes(Sepal.Width, Sepal.Length, colour = Species), data = iris) +
    facet_grid(Species ~ .)
)
viz.positive <- list(
  plot1 = ggplot() +
    theme_bw() +
    theme(panel.margin = grid::unit(2, "lines")) +
    geom_point(aes(Sepal.Width, Sepal.Length, colour = Species), data = iris) +
    facet_grid(Species ~ .)
)
info.zero <- animint2HTML(viz.zero)
info.positive <- animint2HTML(viz.positive)
bg.zero <- getNodeSet(
  info.zero$html,
  '//svg[@id="plot_plot1"]//rect[@class="background_rect"]')
bg.positive <- getNodeSet(
  info.positive$html,
  '//svg[@id="plot_plot1"]//rect[@class="background_rect"]')
test_that("three panels rendered with zero panel.margin", {
  expect_equal(length(bg.zero), 3)
})
test_that("three panels rendered with positive panel.margin", {
  expect_equal(length(bg.positive), 3)
})
test_that("panel y-positions are strictly increasing (vertically stacked)", {
  y.vals <- sort(panel_attr(bg.positive, "y"))
  expect_true(all(diff(y.vals) > 0))
})
test_that("positive panel.margin produces larger inter-panel gap than zero margin", {
  gap.zero <- gap_between_positions(panel_attr(bg.zero, "y"), panel_attr(bg.zero, "height"))
  gap.positive <- gap_between_positions(panel_attr(bg.positive, "y"), panel_attr(bg.positive, "height"))
  expect_gt(gap.positive, 0)
  expect_gt(gap.positive, gap.zero)
})

viz.zero.h <- list(
  plot1 = ggplot() +
    theme_bw() +
    theme(panel.margin = grid::unit(0, "lines")) +
    geom_point(aes(Sepal.Width, Sepal.Length, colour = Species), data = iris) +
    facet_grid(. ~ Species)
)
viz.positive.h <- list(
  plot1 = ggplot() +
    theme_bw() +
    theme(panel.margin = grid::unit(2, "lines")) +
    geom_point(aes(Sepal.Width, Sepal.Length, colour = Species), data = iris) +
    facet_grid(. ~ Species)
)
info.zero.h <- animint2HTML(viz.zero.h)
info.positive.h <- animint2HTML(viz.positive.h)
bg.zero.h <- getNodeSet(
  info.zero.h$html,
  '//svg[@id="plot_plot1"]//rect[@class="background_rect"]')
bg.positive.h <- getNodeSet(
  info.positive.h$html,
  '//svg[@id="plot_plot1"]//rect[@class="background_rect"]')
test_that("facet_grid columns: three panels with zero panel.margin", {
  expect_equal(length(bg.zero.h), 3)
})
test_that("facet_grid columns: three panels with positive panel.margin", {
  expect_equal(length(bg.positive.h), 3)
})
test_that("facet_grid columns: x-positions strictly increasing", {
  x.vals <- sort(panel_attr(bg.positive.h, "x"))
  expect_true(all(diff(x.vals) > 0))
})
test_that("facet_grid columns: positive margin widens horizontal gap", {
  gap.zero <- gap_between_positions(panel_attr(bg.zero.h, "x"), panel_attr(bg.zero.h, "width"))
  gap.positive <- gap_between_positions(panel_attr(bg.positive.h, "x"), panel_attr(bg.positive.h, "width"))
  expect_gt(gap.positive, 0)
  expect_gt(gap.positive, gap.zero)
})

viz.zero.w <- list(
  plot1 = ggplot() +
    theme_bw() +
    theme(panel.margin = grid::unit(0, "lines")) +
    geom_point(aes(Sepal.Width, Sepal.Length, colour = Species), data = iris) +
    facet_wrap(~Species, ncol = 2)
)
viz.positive.w <- list(
  plot1 = ggplot() +
    theme_bw() +
    theme(panel.margin = grid::unit(2, "lines")) +
    geom_point(aes(Sepal.Width, Sepal.Length, colour = Species), data = iris) +
    facet_wrap(~Species, ncol = 2)
)
info.zero.w <- animint2HTML(viz.zero.w)
info.positive.w <- animint2HTML(viz.positive.w)
bg.zero.w <- getNodeSet(
  info.zero.w$html,
  '//svg[@id="plot_plot1"]//rect[@class="background_rect"]')
bg.positive.w <- getNodeSet(
  info.positive.w$html,
  '//svg[@id="plot_plot1"]//rect[@class="background_rect"]')
test_that("facet_wrap: three panels with zero panel.margin", {
  expect_equal(length(bg.zero.w), 3)
})
test_that("facet_wrap: three panels with positive panel.margin", {
  expect_equal(length(bg.positive.w), 3)
})
test_that("facet_wrap: positive margin widens horizontal gap between columns", {
  g0 <- gap_between_positions(panel_attr(bg.zero.w, "x"), panel_attr(bg.zero.w, "width"))
  g1 <- gap_between_positions(panel_attr(bg.positive.w, "x"), panel_attr(bg.positive.w, "width"))
  expect_false(is.na(g0))
  expect_false(is.na(g1))
  expect_gt(g1, 0)
  expect_gt(g1, g0)
})
test_that("facet_wrap: positive margin widens vertical gap between rows", {
  g0 <- gap_between_positions(panel_attr(bg.zero.w, "y"), panel_attr(bg.zero.w, "height"))
  g1 <- gap_between_positions(panel_attr(bg.positive.w, "y"), panel_attr(bg.positive.w, "height"))
  expect_false(is.na(g0))
  expect_false(is.na(g1))
  expect_gt(g1, 0)
  expect_gt(g1, g0)
})

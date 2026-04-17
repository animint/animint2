acontext("panel.margin positive values - Issue #180 - renderer")
library(XML)
panel_x <- function(node_list) {
  attrs <- sapply(node_list, xmlAttrs)
  as.numeric(attrs["x", ])
}
panel_w <- function(node_list) {
  attrs <- sapply(node_list, xmlAttrs)
  as.numeric(attrs["width", ])
}
panel_y <- function(node_list) {
  attrs <- sapply(node_list, xmlAttrs)
  as.numeric(attrs["y", ])
}
panel_h <- function(node_list) {
  attrs <- sapply(node_list, xmlAttrs)
  as.numeric(attrs["height", ])
}
gap_between_panels_y <- function(node_list) {
  y <- sort(panel_y(node_list))
  h <- panel_h(node_list)[order(panel_y(node_list))]
  y[2] - (y[1] + h[1])
}
gap_between_panels_x <- function(node_list) {
  ord <- order(panel_x(node_list))
  x <- panel_x(node_list)[ord]
  w <- panel_w(node_list)[ord]
  x[2] - (x[1] + w[1])
}
vertical_gap_between_wrap_rows <- function(node_list) {
  ys <- panel_y(node_list)
  hs <- panel_h(node_list)
  uy <- sort(unique(ys))
  if (length(uy) < 2) {
    return(NA_real_)
  }
  tol <- 1e-4
  i1 <- abs(ys - uy[1]) < tol
  bottom1 <- max(ys[i1] + hs[i1])
  i2 <- abs(ys - uy[2]) < tol
  top2 <- min(ys[i2])
  top2 - bottom1
}
horizontal_gap_between_wrap_cols <- function(node_list) {
  xs <- panel_x(node_list)
  ws <- panel_w(node_list)
  ux <- sort(unique(xs))
  if (length(ux) < 2) {
    return(NA_real_)
  }
  tol <- 1e-4
  i1 <- abs(xs - ux[1]) < tol
  right1 <- max(xs[i1] + ws[i1])
  i2 <- abs(xs - ux[2]) < tol
  left2 <- min(xs[i2])
  left2 - right1
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
  y.vals <- sort(panel_y(bg.positive))
  expect_true(all(diff(y.vals) > 0))
})
test_that("positive panel.margin produces larger inter-panel gap than zero margin", {
  gap.zero <- gap_between_panels_y(bg.zero)
  gap.positive <- gap_between_panels_y(bg.positive)
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
  x.vals <- sort(panel_x(bg.positive.h))
  expect_true(all(diff(x.vals) > 0))
})
test_that("facet_grid columns: positive margin widens horizontal gap", {
  gap.zero <- gap_between_panels_x(bg.zero.h)
  gap.positive <- gap_between_panels_x(bg.positive.h)
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
  g0 <- horizontal_gap_between_wrap_cols(bg.zero.w)
  g1 <- horizontal_gap_between_wrap_cols(bg.positive.w)
  expect_false(is.na(g0))
  expect_false(is.na(g1))
  expect_gt(g1, 0)
  expect_gt(g1, g0)
})
test_that("facet_wrap: positive margin widens vertical gap between rows", {
  g0 <- vertical_gap_between_wrap_rows(bg.zero.w)
  g1 <- vertical_gap_between_wrap_rows(bg.positive.w)
  expect_false(is.na(g0))
  expect_false(is.na(g1))
  expect_gt(g1, 0)
  expect_gt(g1, g0)
})
